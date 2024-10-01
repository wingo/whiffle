#include <pthread.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "gc-api.h"

#define GC_IMPL 1
#include "gc-internal.h"

#include "background-thread.h"
#include "debug.h"
#include "gc-align.h"
#include "gc-inline.h"
#include "gc-platform.h"
#include "gc-stack.h"
#include "gc-trace.h"
#include "heap-sizer.h"
#include "large-object-space.h"
#include "nofl-space.h"
#if GC_PARALLEL
#include "parallel-tracer.h"
#else
#include "serial-tracer.h"
#endif
#include "spin.h"
#include "mmc-attrs.h"

#define LARGE_OBJECT_THRESHOLD 8192

struct gc_heap {
  struct nofl_space nofl_space;
  struct large_object_space large_object_space;
  struct gc_extern_space *extern_space;
  size_t large_object_pages;
  pthread_mutex_t lock;
  pthread_cond_t collector_cond;
  pthread_cond_t mutator_cond;
  size_t size;
  size_t total_allocated_bytes_at_last_gc;
  size_t size_at_last_gc;
  int collecting;
  int check_pending_ephemerons;
  struct gc_pending_ephemerons *pending_ephemerons;
  struct gc_finalizer_state *finalizer_state;
  enum gc_collection_kind gc_kind;
  size_t mutator_count;
  size_t paused_mutator_count;
  size_t inactive_mutator_count;
  struct gc_heap_roots *roots;
  struct gc_mutator *mutators;
  long count;
  struct gc_tracer tracer;
  double fragmentation_low_threshold;
  double fragmentation_high_threshold;
  double minor_gc_yield_threshold;
  double major_gc_yield_threshold;
  double minimum_major_gc_yield_threshold;
  double pending_ephemerons_size_factor;
  double pending_ephemerons_size_slop;
  struct gc_background_thread *background_thread;
  struct gc_heap_sizer sizer;
  struct gc_event_listener event_listener;
  void *event_listener_data;
};

#define HEAP_EVENT(heap, event, ...)                                    \
  (heap)->event_listener.event((heap)->event_listener_data, ##__VA_ARGS__)
#define MUTATOR_EVENT(mut, event, ...)                                  \
  (mut)->heap->event_listener.event((mut)->event_listener_data, ##__VA_ARGS__)

struct gc_mutator {
  struct nofl_allocator allocator;
  struct gc_heap *heap;
  struct gc_stack stack;
  struct gc_mutator_roots *roots;
  void *event_listener_data;
  struct gc_mutator *next;
  struct gc_mutator *prev;
};

struct gc_trace_worker_data {
  struct nofl_allocator allocator;
};

static inline struct nofl_space*
heap_nofl_space(struct gc_heap *heap) {
  return &heap->nofl_space;
}
static inline struct large_object_space*
heap_large_object_space(struct gc_heap *heap) {
  return &heap->large_object_space;
}
static inline struct gc_extern_space*
heap_extern_space(struct gc_heap *heap) {
  return heap->extern_space;
}
static inline struct gc_heap*
mutator_heap(struct gc_mutator *mutator) {
  return mutator->heap;
}

static void
gc_trace_worker_call_with_data(void (*f)(struct gc_tracer *tracer,
                                         struct gc_heap *heap,
                                         struct gc_trace_worker *worker,
                                         struct gc_trace_worker_data *data),
                               struct gc_tracer *tracer,
                               struct gc_heap *heap,
                               struct gc_trace_worker *worker) {
  struct gc_trace_worker_data data;
  nofl_allocator_reset(&data.allocator);
  f(tracer, heap, worker, &data);
  nofl_allocator_finish(&data.allocator, heap_nofl_space(heap));
}

static inline int
do_trace(struct gc_heap *heap, struct gc_edge edge, struct gc_ref ref,
         struct gc_trace_worker_data *data) {
  if (!gc_ref_is_heap_object(ref))
    return 0;
  if (GC_LIKELY(nofl_space_contains(heap_nofl_space(heap), ref)))
    return nofl_space_evacuate_or_mark_object(heap_nofl_space(heap), edge, ref,
                                              &data->allocator);
  else if (large_object_space_contains(heap_large_object_space(heap), ref))
    return large_object_space_mark_object(heap_large_object_space(heap),
                                          ref);
  else
    return gc_extern_space_visit(heap_extern_space(heap), edge, ref);
}

static inline int
trace_edge(struct gc_heap *heap, struct gc_edge edge,
           struct gc_trace_worker_data *data) {
  struct gc_ref ref = gc_edge_ref(edge);
  int is_new = do_trace(heap, edge, ref, data);

  if (is_new &&
      GC_UNLIKELY(atomic_load_explicit(&heap->check_pending_ephemerons,
                                       memory_order_relaxed)))
    gc_resolve_pending_ephemerons(ref, heap);

  return is_new;
}

int
gc_visit_ephemeron_key(struct gc_edge edge, struct gc_heap *heap) {
  struct gc_ref ref = gc_edge_ref(edge);
  if (!gc_ref_is_heap_object(ref))
    return 0;

  struct nofl_space *nofl_space = heap_nofl_space(heap);
  if (GC_LIKELY(nofl_space_contains(nofl_space, ref)))
    return nofl_space_forward_or_mark_if_traced(nofl_space, edge, ref);

  struct large_object_space *lospace = heap_large_object_space(heap);
  if (large_object_space_contains(lospace, ref))
    return large_object_space_is_copied(lospace, ref);

  GC_CRASH();
}

static int
mutators_are_stopping(struct gc_heap *heap) {
  return atomic_load_explicit(&heap->collecting, memory_order_relaxed);
}

static inline void
heap_lock(struct gc_heap *heap) {
  pthread_mutex_lock(&heap->lock);
}
static inline void
heap_unlock(struct gc_heap *heap) {
  pthread_mutex_unlock(&heap->lock);
}

// with heap lock
static inline int
all_mutators_stopped(struct gc_heap *heap) {
  return heap->mutator_count ==
    heap->paused_mutator_count + heap->inactive_mutator_count;
}

static void
add_mutator(struct gc_heap *heap, struct gc_mutator *mut) {
  mut->heap = heap;
  mut->event_listener_data =
    heap->event_listener.mutator_added(heap->event_listener_data);
  nofl_allocator_reset(&mut->allocator);
  heap_lock(heap);
  // We have no roots.  If there is a GC currently in progress, we have
  // nothing to add.  Just wait until it's done.
  while (mutators_are_stopping(heap))
    pthread_cond_wait(&heap->mutator_cond, &heap->lock);
  mut->next = mut->prev = NULL;
  struct gc_mutator *tail = heap->mutators;
  if (tail) {
    mut->next = tail;
    tail->prev = mut;
  }
  heap->mutators = mut;
  heap->mutator_count++;
  heap_unlock(heap);
}

static void
remove_mutator(struct gc_heap *heap, struct gc_mutator *mut) {
  nofl_allocator_finish(&mut->allocator, heap_nofl_space(heap));
  MUTATOR_EVENT(mut, mutator_removed);
  mut->heap = NULL;
  heap_lock(heap);
  heap->mutator_count--;
  if (mut->next)
    mut->next->prev = mut->prev;
  if (mut->prev)
    mut->prev->next = mut->next;
  else
    heap->mutators = mut->next;
  // We have no roots.  If there is a GC stop currently in progress,
  // maybe tell the controller it can continue.
  if (mutators_are_stopping(heap) && all_mutators_stopped(heap))
    pthread_cond_signal(&heap->collector_cond);
  heap_unlock(heap);
}

void
gc_mutator_set_roots(struct gc_mutator *mut, struct gc_mutator_roots *roots) {
  mut->roots = roots;
}
void
gc_heap_set_roots(struct gc_heap *heap, struct gc_heap_roots *roots) {
  heap->roots = roots;
}
void
gc_heap_set_extern_space(struct gc_heap *heap, struct gc_extern_space *space) {
  heap->extern_space = space;
}

static inline void tracer_visit(struct gc_edge edge, struct gc_heap *heap,
                                void *trace_data) GC_ALWAYS_INLINE;
static inline void
tracer_visit(struct gc_edge edge, struct gc_heap *heap, void *trace_data) {
  struct gc_trace_worker *worker = trace_data;
  if (trace_edge(heap, edge, gc_trace_worker_data(worker)))
    gc_trace_worker_enqueue(worker, gc_edge_ref(edge));
}

static inline struct gc_ref
do_trace_conservative_ref(struct gc_heap *heap, struct gc_conservative_ref ref,
                          int possibly_interior) {
  if (!gc_conservative_ref_might_be_a_heap_object(ref, possibly_interior))
    return gc_ref_null();

  struct nofl_space *nofl_space = heap_nofl_space(heap);
  if (GC_LIKELY(nofl_space_contains_conservative_ref(nofl_space, ref)))
    return nofl_space_mark_conservative_ref(nofl_space, ref, possibly_interior);

  struct large_object_space *lospace = heap_large_object_space(heap);
  return large_object_space_mark_conservative_ref(lospace, ref,
                                                  possibly_interior);
}

static inline struct gc_ref
trace_conservative_ref(struct gc_heap *heap, struct gc_conservative_ref ref,
                       int possibly_interior) {
  struct gc_ref ret = do_trace_conservative_ref(heap, ref, possibly_interior);

  if (gc_ref_is_heap_object(ret) &&
      GC_UNLIKELY(atomic_load_explicit(&heap->check_pending_ephemerons,
                                       memory_order_relaxed)))
    gc_resolve_pending_ephemerons(ret, heap);

  return ret;
}

static inline void
tracer_trace_conservative_ref(struct gc_conservative_ref ref,
                              struct gc_heap *heap,
                              struct gc_trace_worker *worker,
                              int possibly_interior) {
  struct gc_ref resolved = trace_conservative_ref(heap, ref, possibly_interior);
  if (gc_ref_is_heap_object(resolved))
    gc_trace_worker_enqueue(worker, resolved);
}

static inline struct gc_conservative_ref
load_conservative_ref(uintptr_t addr) {
  GC_ASSERT((addr & (sizeof(uintptr_t) - 1)) == 0);
  uintptr_t val;
  memcpy(&val, (char*)addr, sizeof(uintptr_t));
  return gc_conservative_ref(val);
}

static inline void
trace_conservative_edges(uintptr_t low, uintptr_t high, int possibly_interior,
                         struct gc_heap *heap, struct gc_trace_worker *worker) {
  GC_ASSERT(low == align_down(low, sizeof(uintptr_t)));
  GC_ASSERT(high == align_down(high, sizeof(uintptr_t)));
  for (uintptr_t addr = low; addr < high; addr += sizeof(uintptr_t))
    tracer_trace_conservative_ref(load_conservative_ref(addr), heap, worker,
                                  possibly_interior);
}

static inline void
trace_one_conservatively(struct gc_ref ref, struct gc_heap *heap,
                         struct gc_trace_worker *worker) {
  size_t bytes;
  if (GC_LIKELY(nofl_space_contains(heap_nofl_space(heap), ref))) {
    // Generally speaking we trace conservatively and don't allow much
    // in the way of incremental precise marking on a
    // conservative-by-default heap.  But, we make an exception for
    // ephemerons.
    if (GC_UNLIKELY(nofl_is_ephemeron(ref))) {
      gc_trace_ephemeron(gc_ref_heap_object(ref), tracer_visit, heap,
                         worker);
      return;
    }
    bytes = nofl_space_object_size(heap_nofl_space(heap), ref);
  } else {
    bytes = large_object_space_object_size(heap_large_object_space(heap), ref);
  }
  // Intraheap edges are not interior.
  int possibly_interior = 0;
  trace_conservative_edges(gc_ref_value(ref), gc_ref_value(ref) + bytes,
                           possibly_interior, heap, worker);
}

static inline void
trace_one(struct gc_ref ref, struct gc_heap *heap,
          struct gc_trace_worker *worker) {
  if (gc_has_conservative_intraheap_edges())
    trace_one_conservatively(ref, heap, worker);
  else
    gc_trace_object(ref, tracer_visit, heap, worker, NULL);
}

static inline void
trace_root(struct gc_root root, struct gc_heap *heap,
           struct gc_trace_worker *worker) {
  switch (root.kind) {
  case GC_ROOT_KIND_HEAP:
    gc_trace_heap_roots(root.heap->roots, tracer_visit, heap, worker);
    break;
  case GC_ROOT_KIND_MUTATOR:
    gc_trace_mutator_roots(root.mutator->roots, tracer_visit, heap, worker);
    break;
  case GC_ROOT_KIND_CONSERVATIVE_EDGES:
    trace_conservative_edges(root.range.lo_addr, root.range.hi_addr, 0,
                             heap, worker);
    break;
  case GC_ROOT_KIND_CONSERVATIVE_POSSIBLY_INTERIOR_EDGES:
    trace_conservative_edges(root.range.lo_addr, root.range.hi_addr, 1,
                             heap, worker);
    break;
  case GC_ROOT_KIND_RESOLVED_EPHEMERONS:
    gc_trace_resolved_ephemerons(root.resolved_ephemerons, tracer_visit,
                                 heap, worker);
    break;
  case GC_ROOT_KIND_EDGE:
    tracer_visit(root.edge, heap, worker);
    break;
  case GC_ROOT_KIND_REMEMBERED_OBJECT:
    trace_one(root.ref, heap, worker);
    break;
  case GC_ROOT_KIND_REMEMBERED_SLAB:
    nofl_space_trace_remembered_slab(heap_nofl_space(heap), root.idx,
                                     trace_one, heap, worker);
    break;
  default:
    GC_CRASH();
  }
}

static void
request_mutators_to_stop(struct gc_heap *heap) {
  GC_ASSERT(!mutators_are_stopping(heap));
  atomic_store_explicit(&heap->collecting, 1, memory_order_relaxed);
}

static void
allow_mutators_to_continue(struct gc_heap *heap) {
  GC_ASSERT(mutators_are_stopping(heap));
  GC_ASSERT(all_mutators_stopped(heap));
  heap->paused_mutator_count--;
  atomic_store_explicit(&heap->collecting, 0, memory_order_relaxed);
  GC_ASSERT(!mutators_are_stopping(heap));
  pthread_cond_broadcast(&heap->mutator_cond);
}

static void
heap_reset_large_object_pages(struct gc_heap *heap, size_t npages) {
  size_t previous = heap->large_object_pages;
  heap->large_object_pages = npages;
  GC_ASSERT(npages <= previous);
  size_t bytes = (previous - npages) <<
    heap_large_object_space(heap)->page_size_log2;
  // If heap size is fixed, we won't need to allocate any more nofl blocks, as
  // nothing uses paged-out blocks except large object allocation.  But if the
  // heap can grow, growth can consume nofl-space blocks that were paged out to
  // allow for lospace allocations, which means that here we may need to
  // allocate additional slabs.
  nofl_space_expand(heap_nofl_space(heap), bytes);
}

static void
wait_for_mutators_to_stop(struct gc_heap *heap) {
  heap->paused_mutator_count++;
  while (!all_mutators_stopped(heap))
    pthread_cond_wait(&heap->collector_cond, &heap->lock);
}

static enum gc_collection_kind
pause_mutator_for_collection(struct gc_heap *heap,
                             struct gc_mutator *mut) GC_NEVER_INLINE;
static enum gc_collection_kind
pause_mutator_for_collection(struct gc_heap *heap, struct gc_mutator *mut) {
  GC_ASSERT(mutators_are_stopping(heap));
  GC_ASSERT(!all_mutators_stopped(heap));
  MUTATOR_EVENT(mut, mutator_stopping);
  MUTATOR_EVENT(mut, mutator_stopped);
  heap->paused_mutator_count++;
  enum gc_collection_kind collection_kind = heap->gc_kind;
  if (all_mutators_stopped(heap))
    pthread_cond_signal(&heap->collector_cond);

  do
    pthread_cond_wait(&heap->mutator_cond, &heap->lock);
  while (mutators_are_stopping(heap));
  heap->paused_mutator_count--;

  MUTATOR_EVENT(mut, mutator_restarted);
  return collection_kind;
}

static void
resize_heap(struct gc_heap *heap, size_t new_size) {
  if (new_size == heap->size)
    return;
  DEBUG("------ resizing heap\n");
  DEBUG("------ old heap size: %zu bytes\n", heap->size);
  DEBUG("------ new heap size: %zu bytes\n", new_size);
  if (new_size < heap->size)
    nofl_space_shrink(heap_nofl_space(heap), heap->size - new_size);
  else
    nofl_space_expand(heap_nofl_space(heap), new_size - heap->size);

  heap->size = new_size;
  HEAP_EVENT(heap, heap_resized, new_size);
}

static double
heap_last_gc_yield(struct gc_heap *heap) {
  size_t live_size =
    nofl_space_live_size_at_last_collection(heap_nofl_space(heap)) +
    large_object_space_size_at_last_collection(heap_large_object_space(heap));

  if (live_size > heap->size_at_last_gc)
    return 0;
  return 1.0 - ((double) live_size) / heap->size_at_last_gc;
}

static double
heap_fragmentation(struct gc_heap *heap) {
  struct nofl_space *nofl_space = heap_nofl_space(heap);
  size_t fragmentation = nofl_space_fragmentation(nofl_space);
  return ((double)fragmentation) / heap->size;
}

static size_t
heap_estimate_live_data_after_gc(struct gc_heap *heap,
                                 size_t last_live_bytes,
                                 double last_yield) {
  size_t bytes =
    nofl_space_estimate_live_bytes_after_gc(heap_nofl_space(heap),
                                            last_yield)
    + large_object_space_size_at_last_collection(heap_large_object_space(heap));
  if (bytes < last_live_bytes)
    return last_live_bytes;
  return bytes;
}

static void
detect_out_of_memory(struct gc_heap *heap, uintptr_t allocation_since_last_gc) {
  if (heap->sizer.policy != GC_HEAP_SIZE_FIXED)
    return;

  if (allocation_since_last_gc > nofl_space_fragmentation(heap_nofl_space(heap)))
    return;

  if (heap->gc_kind == GC_COLLECTION_MINOR)
    return;

  // No allocation since last gc: out of memory.
  fprintf(stderr, "ran out of space, heap size %zu\n", heap->size);
  GC_CRASH();
}

static double
clamp_major_gc_yield_threshold(struct gc_heap *heap, double threshold) {
  if (threshold < heap->minimum_major_gc_yield_threshold)
    threshold = heap->minimum_major_gc_yield_threshold;
  double one_block = NOFL_BLOCK_SIZE * 1.0 / heap->size;
  if (threshold < one_block)
    threshold = one_block;
  return threshold;
}

static enum gc_collection_kind
determine_collection_kind(struct gc_heap *heap,
                          enum gc_collection_kind requested) {
  struct nofl_space *nofl_space = heap_nofl_space(heap);
  enum gc_collection_kind previous_gc_kind = atomic_load(&heap->gc_kind);
  enum gc_collection_kind gc_kind;
  double yield = heap_last_gc_yield(heap);
  double fragmentation = heap_fragmentation(heap);
  ssize_t pending = atomic_load_explicit(&nofl_space->pending_unavailable_bytes,
                                         memory_order_acquire);

  if (heap->count == 0) {
    DEBUG("first collection is always major\n");
    gc_kind = GC_COLLECTION_MAJOR;
  } else if (requested != GC_COLLECTION_ANY) {
    DEBUG("user specifically requested collection kind %d\n", (int)requested);
    gc_kind = requested;
  } else if (pending > 0) {
    DEBUG("evacuating due to need to reclaim %zd bytes\n", pending);
    // During the last cycle, a large allocation could not find enough
    // free blocks, and we decided not to expand the heap.  Let's do an
    // evacuating major collection to maximize the free block yield.
    gc_kind = GC_COLLECTION_COMPACTING;
  } else if (previous_gc_kind == GC_COLLECTION_COMPACTING
             && fragmentation >= heap->fragmentation_low_threshold) {
    DEBUG("continuing evacuation due to fragmentation %.2f%% > %.2f%%\n",
          fragmentation * 100.,
          heap->fragmentation_low_threshold * 100.);
    // For some reason, we already decided to compact in the past,
    // and fragmentation hasn't yet fallen below a low-water-mark.
    // Keep going.
    gc_kind = GC_COLLECTION_COMPACTING;
  } else if (fragmentation > heap->fragmentation_high_threshold) {
    // Switch to evacuation mode if the heap is too fragmented.
    DEBUG("triggering compaction due to fragmentation %.2f%% > %.2f%%\n",
          fragmentation * 100.,
          heap->fragmentation_high_threshold * 100.);
    gc_kind = GC_COLLECTION_COMPACTING;
  } else if (previous_gc_kind == GC_COLLECTION_COMPACTING) {
    // We were evacuating, but we're good now.  Go back to minor
    // collections.
    DEBUG("returning to in-place collection, fragmentation %.2f%% < %.2f%%\n",
          fragmentation * 100.,
          heap->fragmentation_low_threshold * 100.);
    gc_kind = GC_GENERATIONAL ? GC_COLLECTION_MINOR : GC_COLLECTION_MAJOR;
  } else if (!GC_GENERATIONAL) {
    DEBUG("keeping on with major in-place GC\n");
    GC_ASSERT(previous_gc_kind == GC_COLLECTION_MAJOR);
    gc_kind = GC_COLLECTION_MAJOR;
  } else if (previous_gc_kind != GC_COLLECTION_MINOR) {
    DEBUG("returning to minor collection\n");
    // Go back to minor collections.
    gc_kind = GC_COLLECTION_MINOR;
  } else if (yield < heap->major_gc_yield_threshold) {
    DEBUG("collection yield too low, triggering major collection\n");
    // Nursery is getting tight; trigger a major GC.
    gc_kind = GC_COLLECTION_MAJOR;
  } else {
    DEBUG("keeping on with minor GC\n");
    // Nursery has adequate space; keep trucking with minor GCs.
    GC_ASSERT(previous_gc_kind == GC_COLLECTION_MINOR);
    gc_kind = GC_COLLECTION_MINOR;
  }

  if (gc_has_conservative_intraheap_edges() &&
      gc_kind == GC_COLLECTION_COMPACTING) {
    DEBUG("welp.  conservative heap scanning, no evacuation for you\n");
    gc_kind = GC_COLLECTION_MAJOR;
  }

  // If this is the first in a series of minor collections, reset the
  // threshold at which we should do a major GC.
  if (gc_kind == GC_COLLECTION_MINOR &&
      previous_gc_kind != GC_COLLECTION_MINOR) {
    double yield = heap_last_gc_yield(heap);
    double threshold = yield * heap->minor_gc_yield_threshold;
    double clamped = clamp_major_gc_yield_threshold(heap, threshold);
    heap->major_gc_yield_threshold = clamped;
    DEBUG("first minor collection at yield %.2f%%, threshold %.2f%%\n",
          yield * 100., clamped * 100.);
  }

  atomic_store(&heap->gc_kind, gc_kind);
  return gc_kind;
}

static void
enqueue_conservative_roots(uintptr_t low, uintptr_t high,
                           struct gc_heap *heap, void *data) {
  int *possibly_interior = data;
  gc_tracer_add_root(&heap->tracer,
                     gc_root_conservative_edges(low, high, *possibly_interior));
}

static void
enqueue_mutator_conservative_roots(struct gc_heap *heap) {
  if (gc_has_mutator_conservative_roots()) {
    int possibly_interior = gc_mutator_conservative_roots_may_be_interior();
    for (struct gc_mutator *mut = heap->mutators;
         mut;
         mut = mut->next)
      gc_stack_visit(&mut->stack, enqueue_conservative_roots, heap,
                     &possibly_interior);
  }
}

static void
enqueue_global_conservative_roots(struct gc_heap *heap) {
  if (gc_has_global_conservative_roots()) {
    int possibly_interior = 0;
    gc_platform_visit_global_conservative_roots
      (enqueue_conservative_roots, heap, &possibly_interior);
  }
}

static void
enqueue_pinned_roots(struct gc_heap *heap) {
  GC_ASSERT(!heap_nofl_space(heap)->evacuating);
  enqueue_mutator_conservative_roots(heap);
  enqueue_global_conservative_roots(heap);
}

static void
enqueue_root_edge(struct gc_edge edge, struct gc_heap *heap, void *unused) {
  gc_tracer_add_root(&heap->tracer, gc_root_edge(edge));
}

static void
enqueue_remembered_object(struct gc_ref ref, struct gc_heap *heap) {
  gc_tracer_add_root(&heap->tracer, gc_root_remembered_object(ref));
}

static void
enqueue_generational_roots(struct gc_heap *heap,
                           enum gc_collection_kind gc_kind) {
  if (!GC_GENERATIONAL) return;
  if (gc_kind == GC_COLLECTION_MINOR) {
    for (size_t i = 0; i < heap_nofl_space(heap)->nslabs; i++)
      gc_tracer_add_root(&heap->tracer, gc_root_remembered_slab(i));
    large_object_space_trace_remembered_set(heap_large_object_space(heap),
                                            enqueue_remembered_object,
                                            heap);
  } else {
    nofl_space_clear_remembered_set(heap_nofl_space(heap));
    large_object_space_clear_remembered_set(heap_large_object_space(heap));
  }
}

static void
enqueue_relocatable_roots(struct gc_heap *heap,
                          enum gc_collection_kind gc_kind) {
  for (struct gc_mutator *mut = heap->mutators;
       mut;
       mut = mut->next) {
    if (mut->roots)
      gc_tracer_add_root(&heap->tracer, gc_root_mutator(mut));
  }
  if (heap->roots)
    gc_tracer_add_root(&heap->tracer, gc_root_heap(heap));
  gc_visit_finalizer_roots(heap->finalizer_state, enqueue_root_edge, heap, NULL);
  enqueue_generational_roots(heap, gc_kind);
}

static void
resolve_ephemerons_lazily(struct gc_heap *heap) {
  atomic_store_explicit(&heap->check_pending_ephemerons, 0,
                        memory_order_release);
}

static void
resolve_ephemerons_eagerly(struct gc_heap *heap) {
  atomic_store_explicit(&heap->check_pending_ephemerons, 1,
                        memory_order_release);
  gc_scan_pending_ephemerons(heap->pending_ephemerons, heap, 0, 1);
}

static void
trace_resolved_ephemerons(struct gc_heap *heap) {
  for (struct gc_ephemeron *resolved = gc_pop_resolved_ephemerons(heap);
       resolved;
       resolved = gc_pop_resolved_ephemerons(heap)) {
    gc_tracer_add_root(&heap->tracer, gc_root_resolved_ephemerons(resolved));
    gc_tracer_trace(&heap->tracer);
  }
}

static void
resolve_finalizers(struct gc_heap *heap) {
  for (size_t priority = 0;
       priority < gc_finalizer_priority_count();
       priority++) {
    if (gc_resolve_finalizers(heap->finalizer_state, priority,
                              enqueue_root_edge, heap, NULL)) {
      gc_tracer_trace(&heap->tracer);
      trace_resolved_ephemerons(heap);
    }
  }
  gc_notify_finalizers(heap->finalizer_state, heap);
}

static void
sweep_ephemerons(struct gc_heap *heap) {
  return gc_sweep_pending_ephemerons(heap->pending_ephemerons, 0, 1);
}

static void collect(struct gc_mutator *mut,
                    enum gc_collection_kind requested_kind,
                    int requested_by_user) GC_NEVER_INLINE;
static void
collect(struct gc_mutator *mut, enum gc_collection_kind requested_kind,
        int requested_by_user) {
  struct gc_heap *heap = mutator_heap(mut);
  struct nofl_space *nofl_space = heap_nofl_space(heap);
  struct large_object_space *lospace = heap_large_object_space(heap);
  struct gc_extern_space *exspace = heap_extern_space(heap);
  uint64_t start_ns = gc_platform_monotonic_nanoseconds();
  MUTATOR_EVENT(mut, mutator_cause_gc);
  DEBUG("start collect #%ld:\n", heap->count);
  HEAP_EVENT(heap, requesting_stop);
  request_mutators_to_stop(heap);
  nofl_finish_sweeping(&mut->allocator, nofl_space);
  HEAP_EVENT(heap, waiting_for_stop);
  wait_for_mutators_to_stop(heap);
  HEAP_EVENT(heap, mutators_stopped);
  uint64_t allocation_counter = 0;
  nofl_space_add_to_allocation_counter(nofl_space, &allocation_counter);
  large_object_space_add_to_allocation_counter(lospace, &allocation_counter);
  heap->total_allocated_bytes_at_last_gc += allocation_counter;
  if (!requested_by_user)
    detect_out_of_memory(heap, allocation_counter);
  enum gc_collection_kind gc_kind =
    determine_collection_kind(heap, requested_kind);
  int is_minor = gc_kind == GC_COLLECTION_MINOR;
  HEAP_EVENT(heap, prepare_gc, gc_kind);
  nofl_space_prepare_gc(nofl_space, gc_kind);
  large_object_space_start_gc(lospace, is_minor);
  gc_extern_space_start_gc(exspace, is_minor);
  resolve_ephemerons_lazily(heap);
  gc_tracer_prepare(&heap->tracer);
  double yield = heap_last_gc_yield(heap);
  double fragmentation = heap_fragmentation(heap);
  size_t live_bytes = heap->size * (1.0 - yield);
  HEAP_EVENT(heap, live_data_size, live_bytes);
  DEBUG("last gc yield: %f; fragmentation: %f\n", yield, fragmentation);
  enqueue_pinned_roots(heap);
  // Eagerly trace pinned roots if we are going to relocate objects.
  if (gc_kind == GC_COLLECTION_COMPACTING)
    gc_tracer_trace_roots(&heap->tracer);
  // Process the rest of the roots in parallel.  This heap event should probably
  // be removed, as there is no clear cutoff time.
  HEAP_EVENT(heap, roots_traced);
  enqueue_relocatable_roots(heap, gc_kind);
  nofl_space_start_gc(nofl_space, gc_kind);
  gc_tracer_trace(&heap->tracer);
  HEAP_EVENT(heap, heap_traced);
  resolve_ephemerons_eagerly(heap);
  trace_resolved_ephemerons(heap);
  HEAP_EVENT(heap, ephemerons_traced);
  resolve_finalizers(heap);
  HEAP_EVENT(heap, finalizers_traced);
  sweep_ephemerons(heap);
  gc_tracer_release(&heap->tracer);
  nofl_space_finish_gc(nofl_space, gc_kind);
  large_object_space_finish_gc(lospace, is_minor);
  gc_extern_space_finish_gc(exspace, is_minor);
  heap->count++;
  heap_reset_large_object_pages(heap, lospace->live_pages_at_last_collection);
  uint64_t pause_ns = gc_platform_monotonic_nanoseconds() - start_ns;
  size_t live_bytes_estimate =
    heap_estimate_live_data_after_gc(heap, live_bytes, yield);
  DEBUG("--- total live bytes estimate: %zu\n", live_bytes_estimate);
  gc_heap_sizer_on_gc(heap->sizer, heap->size, live_bytes_estimate, pause_ns,
                      resize_heap);
  heap->size_at_last_gc = heap->size;
  HEAP_EVENT(heap, restarting_mutators);
  allow_mutators_to_continue(heap);
}

static void
trigger_collection(struct gc_mutator *mut,
                   enum gc_collection_kind requested_kind,
                   int requested_by_user) {
  struct gc_heap *heap = mutator_heap(mut);
  int prev_kind = -1;
  gc_stack_capture_hot(&mut->stack);
  nofl_allocator_finish(&mut->allocator, heap_nofl_space(heap));
  heap_lock(heap);
  while (mutators_are_stopping(heap))
    prev_kind = pause_mutator_for_collection(heap, mut);
  if (prev_kind < (int)requested_kind)
    collect(mut, requested_kind, requested_by_user);
  heap_unlock(heap);
}

void
gc_collect(struct gc_mutator *mut, enum gc_collection_kind kind) {
  trigger_collection(mut, kind, 1);
}

int*
gc_safepoint_flag_loc(struct gc_mutator *mut) {
  return &mutator_heap(mut)->collecting;
}

void
gc_safepoint_slow(struct gc_mutator *mut) {
  struct gc_heap *heap = mutator_heap(mut);
  gc_stack_capture_hot(&mut->stack);
  nofl_allocator_finish(&mut->allocator, heap_nofl_space(heap));
  heap_lock(heap);
  while (mutators_are_stopping(mutator_heap(mut)))
    pause_mutator_for_collection(heap, mut);
  heap_unlock(heap);
}

static void*
allocate_large(struct gc_mutator *mut, size_t size) {
  struct gc_heap *heap = mutator_heap(mut);
  struct nofl_space *nofl_space = heap_nofl_space(heap);
  struct large_object_space *lospace = heap_large_object_space(heap);

  size_t npages = large_object_space_npages(lospace, size);

  nofl_space_request_release_memory(nofl_space,
                                    npages << lospace->page_size_log2);

  while (!nofl_space_shrink(nofl_space, 0))
    trigger_collection(mut, GC_COLLECTION_COMPACTING, 0);
  atomic_fetch_add(&heap->large_object_pages, npages);

  void *ret = large_object_space_alloc(lospace, npages);
  if (!ret)
    ret = large_object_space_obtain_and_alloc(lospace, npages);

  if (!ret) {
    perror("weird: we have the space but mmap didn't work");
    GC_CRASH();
  }

  return ret;
}

static void
collect_for_small_allocation(void *mut) {
  trigger_collection(mut, GC_COLLECTION_ANY, 0);
}

void*
gc_allocate_slow(struct gc_mutator *mut, size_t size) {
  GC_ASSERT(size > 0); // allocating 0 bytes would be silly

  if (size > gc_allocator_large_threshold())
    return allocate_large(mut, size);

  return gc_ref_heap_object(nofl_allocate(&mut->allocator,
                                          heap_nofl_space(mutator_heap(mut)),
                                          size, collect_for_small_allocation,
                                          mut));
}

void*
gc_allocate_pointerless(struct gc_mutator *mut, size_t size) {
  return gc_allocate(mut, size);
}

void
gc_pin_object(struct gc_mutator *mut, struct gc_ref ref) {
  struct nofl_space *nofl = heap_nofl_space(mutator_heap(mut));
  if (nofl_space_contains(nofl, ref))
    nofl_space_pin_object(nofl, ref);
  // Otherwise if it's a large or external object, it won't move.
}

void
gc_write_barrier_slow(struct gc_mutator *mut, struct gc_ref obj,
                      size_t obj_size, struct gc_edge edge,
                      struct gc_ref new_val) {
  if (!GC_GENERATIONAL) return;
  GC_ASSERT(obj_size > gc_allocator_large_threshold());
  struct gc_heap *heap = mutator_heap(mut);
  struct large_object_space *space = heap_large_object_space(heap);
  if (!large_object_space_is_old(space, obj))
    return;
  if (gc_object_set_remembered(obj))
    large_object_space_remember_object(space, obj);
}
  
struct gc_ephemeron*
gc_allocate_ephemeron(struct gc_mutator *mut) {
  struct gc_ref ret =
    gc_ref_from_heap_object(gc_allocate(mut, gc_ephemeron_size()));
  nofl_space_set_ephemeron_flag(ret);
  return gc_ref_heap_object(ret);
}

void
gc_ephemeron_init(struct gc_mutator *mut, struct gc_ephemeron *ephemeron,
                  struct gc_ref key, struct gc_ref value) {
  gc_ephemeron_init_internal(mutator_heap(mut), ephemeron, key, value);
  // No write barrier: we require that the ephemeron be newer than the
  // key or the value.
}

struct gc_pending_ephemerons *
gc_heap_pending_ephemerons(struct gc_heap *heap) {
  return heap->pending_ephemerons;
}

unsigned
gc_heap_ephemeron_trace_epoch(struct gc_heap *heap) {
  return heap->count;
}

struct gc_finalizer*
gc_allocate_finalizer(struct gc_mutator *mut) {
  return gc_allocate(mut, gc_finalizer_size());
}

void
gc_finalizer_attach(struct gc_mutator *mut, struct gc_finalizer *finalizer,
                    unsigned priority, struct gc_ref object,
                    struct gc_ref closure) {
  gc_finalizer_init_internal(finalizer, object, closure);
  gc_finalizer_attach_internal(mutator_heap(mut)->finalizer_state,
                               finalizer, priority);
  // No write barrier.
}

struct gc_finalizer*
gc_pop_finalizable(struct gc_mutator *mut) {
  return gc_finalizer_state_pop(mutator_heap(mut)->finalizer_state);
}

void
gc_set_finalizer_callback(struct gc_heap *heap,
                               gc_finalizer_callback callback) {
  gc_finalizer_state_set_callback(heap->finalizer_state, callback);
}

static int
heap_prepare_pending_ephemerons(struct gc_heap *heap) {
  struct gc_pending_ephemerons *cur = heap->pending_ephemerons;
  size_t target = heap->size * heap->pending_ephemerons_size_factor;
  double slop = heap->pending_ephemerons_size_slop;

  heap->pending_ephemerons = gc_prepare_pending_ephemerons(cur, target, slop);

  return !!heap->pending_ephemerons;
}

struct gc_options {
  struct gc_common_options common;
};

int
gc_option_from_string(const char *str) {
  return gc_common_option_from_string(str);
}

struct gc_options*
gc_allocate_options(void) {
  struct gc_options *ret = malloc(sizeof(struct gc_options));
  gc_init_common_options(&ret->common);
  return ret;
}

int
gc_options_set_int(struct gc_options *options, int option, int value) {
  return gc_common_options_set_int(&options->common, option, value);
}

int
gc_options_set_size(struct gc_options *options, int option,
                    size_t value) {
  return gc_common_options_set_size(&options->common, option, value);
}

int
gc_options_set_double(struct gc_options *options, int option,
                      double value) {
  return gc_common_options_set_double(&options->common, option, value);
}

int
gc_options_parse_and_set(struct gc_options *options, int option,
                         const char *value) {
  return gc_common_options_parse_and_set(&options->common, option, value);
}

static uint64_t allocation_counter_from_thread(struct gc_heap *heap) {
  uint64_t ret = heap->total_allocated_bytes_at_last_gc;
  if (pthread_mutex_trylock(&heap->lock)) return ret;
  nofl_space_add_to_allocation_counter(heap_nofl_space(heap), &ret);
  large_object_space_add_to_allocation_counter(heap_large_object_space(heap),
                                               &ret);
  pthread_mutex_unlock(&heap->lock);
  return ret;
}

static void set_heap_size_from_thread(struct gc_heap *heap, size_t size) {
  if (pthread_mutex_trylock(&heap->lock)) return;
  resize_heap(heap, size);
  pthread_mutex_unlock(&heap->lock);
}

static int
heap_init(struct gc_heap *heap, const struct gc_options *options) {
  // *heap is already initialized to 0.

  pthread_mutex_init(&heap->lock, NULL);
  pthread_cond_init(&heap->mutator_cond, NULL);
  pthread_cond_init(&heap->collector_cond, NULL);
  heap->size = heap->size_at_last_gc = options->common.heap_size;

  if (!gc_tracer_init(&heap->tracer, heap, options->common.parallelism))
    GC_CRASH();

  heap->pending_ephemerons_size_factor = 0.005;
  heap->pending_ephemerons_size_slop = 0.5;
  heap->fragmentation_low_threshold = 0.05;
  heap->fragmentation_high_threshold = 0.10;
  heap->minor_gc_yield_threshold = 0.30;
  heap->minimum_major_gc_yield_threshold = 0.05;
  heap->major_gc_yield_threshold =
    clamp_major_gc_yield_threshold(heap, heap->minor_gc_yield_threshold);

  if (!heap_prepare_pending_ephemerons(heap))
    GC_CRASH();

  heap->finalizer_state = gc_make_finalizer_state();
  if (!heap->finalizer_state)
    GC_CRASH();

  heap->background_thread = gc_make_background_thread();
  heap->sizer = gc_make_heap_sizer(heap, &options->common,
                                   allocation_counter_from_thread,
                                   set_heap_size_from_thread,
                                   heap->background_thread);

  return 1;
}

int
gc_init(const struct gc_options *options, struct gc_stack_addr *stack_base,
        struct gc_heap **heap, struct gc_mutator **mut,
        struct gc_event_listener event_listener,
        void *event_listener_data) {
  GC_ASSERT_EQ(gc_allocator_small_granule_size(), NOFL_GRANULE_SIZE);
  GC_ASSERT_EQ(gc_allocator_large_threshold(), LARGE_OBJECT_THRESHOLD);
  GC_ASSERT_EQ(gc_allocator_allocation_pointer_offset(),
               offsetof(struct nofl_allocator, alloc));
  GC_ASSERT_EQ(gc_allocator_allocation_limit_offset(),
               offsetof(struct nofl_allocator, sweep));
  GC_ASSERT_EQ(gc_allocator_alloc_table_alignment(), NOFL_SLAB_SIZE);
  GC_ASSERT_EQ(gc_allocator_alloc_table_begin_pattern(), NOFL_METADATA_BYTE_YOUNG);
  GC_ASSERT_EQ(gc_allocator_alloc_table_end_pattern(), NOFL_METADATA_BYTE_END);
  if (GC_GENERATIONAL) {
    GC_ASSERT_EQ(gc_write_barrier_card_table_alignment(), NOFL_SLAB_SIZE);
    GC_ASSERT_EQ(gc_write_barrier_card_size(),
                 NOFL_BLOCK_SIZE / NOFL_REMSET_BYTES_PER_BLOCK);
  }

  *heap = calloc(1, sizeof(struct gc_heap));
  if (!*heap) GC_CRASH();

  if (!heap_init(*heap, options))
    GC_CRASH();

  (*heap)->event_listener = event_listener;
  (*heap)->event_listener_data = event_listener_data;
  HEAP_EVENT(*heap, init, (*heap)->size);

  struct nofl_space *space = heap_nofl_space(*heap);
  if (!nofl_space_init(space, (*heap)->size,
                       options->common.parallelism != 1,
                       (*heap)->fragmentation_low_threshold,
                       (*heap)->background_thread)) {
    free(*heap);
    *heap = NULL;
    return 0;
  }
  
  if (!large_object_space_init(heap_large_object_space(*heap), *heap))
    GC_CRASH();

  *mut = calloc(1, sizeof(struct gc_mutator));
  if (!*mut) GC_CRASH();
  gc_stack_init(&(*mut)->stack, stack_base);
  add_mutator(*heap, *mut);

  gc_background_thread_start((*heap)->background_thread);
  
  return 1;
}

struct gc_mutator*
gc_init_for_thread(struct gc_stack_addr *stack_base,
                                      struct gc_heap *heap) {
  struct gc_mutator *ret = calloc(1, sizeof(struct gc_mutator));
  if (!ret)
    GC_CRASH();
  gc_stack_init(&ret->stack, stack_base);
  add_mutator(heap, ret);
  return ret;
}

void
gc_finish_for_thread(struct gc_mutator *mut) {
  remove_mutator(mutator_heap(mut), mut);
  free(mut);
}

static void
deactivate_mutator(struct gc_heap *heap, struct gc_mutator *mut) {
  GC_ASSERT(mut->next == NULL);
  nofl_allocator_finish(&mut->allocator, heap_nofl_space(heap));
  heap_lock(heap);
  heap->inactive_mutator_count++;
  gc_stack_capture_hot(&mut->stack);
  if (all_mutators_stopped(heap))
    pthread_cond_signal(&heap->collector_cond);
  heap_unlock(heap);
}

static void
reactivate_mutator(struct gc_heap *heap, struct gc_mutator *mut) {
  heap_lock(heap);
  while (mutators_are_stopping(heap))
    pthread_cond_wait(&heap->mutator_cond, &heap->lock);
  heap->inactive_mutator_count--;
  heap_unlock(heap);
}

void*
gc_call_without_gc(struct gc_mutator *mut, void* (*f)(void*), void *data) {
  struct gc_heap *heap = mutator_heap(mut);
  deactivate_mutator(heap, mut);
  void *ret = f(data);
  reactivate_mutator(heap, mut);
  return ret;
}
