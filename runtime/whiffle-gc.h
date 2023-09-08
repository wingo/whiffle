#ifndef WHIFFLE_EMBEDDER_H
#define WHIFFLE_EMBEDDER_H

#include <stdatomic.h>
#include <stddef.h>

#include "whiffle/types.h"
#include "gc-config.h"
#include "gc-embedder-api.h"

#define GC_EMBEDDER_EPHEMERON_HEADER Tagged tag;

static inline int
gc_is_valid_conservative_ref_displacement(uintptr_t displacement) {
#if GC_CONSERVATIVE_ROOTS || GC_CONSERVATIVE_TRACE
  return displacement == 0 || displacement == PAIR_TAG;
#else
  // Shouldn't get here.
  GC_CRASH();
#endif
}

static inline int gc_extern_space_visit(struct gc_extern_space *state,
                                        struct gc_edge edge,
                                        struct gc_ref ref) {
  // Statically allocated objects can never reference objects from
  // another space; it's mostly fine to consider them all to be
  // precolored black.  Only problem is that pending ephemerons with REF
  // as key will not be resolved as live.
  return 0;
}
static inline void gc_extern_space_start_gc(struct gc_extern_space *space,
                                            int is_minor_gc) {
}
static inline void gc_extern_space_finish_gc(struct gc_extern_space *space,
                                            int is_minor_gc) {
}

static inline void trace_tagged_edge(void (*trace_edge)(struct gc_edge edge,
                                                        struct gc_heap *heap,
                                                        void *trace_data),
                                     struct gc_edge edge,
                                     struct gc_heap *heap,
                                     void *trace_data) {
  Value v = { gc_ref_value(gc_edge_ref(edge)) };
  if (is_heap_object(v))
    trace_edge(edge, heap, trace_data);
}

static inline void gc_trace_object(struct gc_ref ref,
                                   void (*trace_edge)(struct gc_edge edge,
                                                      struct gc_heap *heap,
                                                      void *trace_data),
                                   struct gc_heap *heap,
                                   void *trace_data,
                                   size_t *size) {
#if GC_CONSERVATIVE_TRACE
  // Shouldn't get here.
  GC_CRASH();
#else
  if (tagged_is_pair(gc_ref_heap_object(ref))) {
    Pair *p = gc_ref_heap_object(ref);
    if (trace_edge) {
      trace_tagged_edge(trace_edge, gc_edge(&p->tag), heap, trace_data);
      trace_tagged_edge(trace_edge, gc_edge(&p->cdr), heap, trace_data);
    }
    if (size)
      *size = sizeof(*p);
    return;
  }

  switch (tagged_kind(gc_ref_heap_object(ref))) {
    case BOX_TAG: {
      Box *b = gc_ref_heap_object(ref);
      if (trace_edge) {
        trace_tagged_edge(trace_edge, gc_edge(&b->val), heap, trace_data);
      }
      if (size)
        *size = sizeof(*b);
      return;
    }

    case VECTOR_TAG: {
      Vector *v = gc_ref_heap_object(ref);
      size_t len = tagged_payload(&v->tag);
      if (trace_edge) {
        for (size_t i = 0; i < len; i++)
          trace_tagged_edge(trace_edge, gc_edge(&v->vals[i]), heap, trace_data);
      }
      if (size)
        *size = sizeof(*v) + sizeof(Value) * len;
      return;
    }

    case CLOSURE_TAG: {
      Closure *c = gc_ref_heap_object(ref);
      size_t nfree = tagged_payload(&c->tag);
      if (trace_edge) {
        for (size_t i = 0; i < nfree; i++)
          trace_tagged_edge(trace_edge, gc_edge(&c->free_vars[i]), heap,
                            trace_data);
      }
      if (size)
        *size = sizeof(*c) + sizeof(Value) * nfree;
      return;
    }

    case STRING_TAG: {
      String *str = gc_ref_heap_object(ref);
      if (trace_edge) {
        trace_edge(gc_edge(&str->chars), heap, trace_data);
      }
      if (size)
        *size = sizeof(*str);
      return;
    }

    case SYMBOL_TAG: {
      Symbol *sym = gc_ref_heap_object(ref);
      if (trace_edge) {
        trace_edge(gc_edge(&sym->str), heap, trace_data);
      }
      if (size)
        *size = sizeof(*sym);
      return;
    }

    case THREAD_TAG: {
      ThreadHandle *thr = gc_ref_heap_object(ref);
      if (size)
        *size = sizeof(*thr);
      return;
    }

    default:
      GC_CRASH();
  }
#endif // GC_CONSERVATIVE_TRACE
}

static inline void gc_trace_mutator_roots(struct gc_mutator_roots *roots,
                                          void (*trace_edge)(struct gc_edge edge,
                                                             struct gc_heap *heap,
                                                             void *trace_data),
                                          struct gc_heap *heap,
                                          void *trace_data) {
  if (roots) {
    for (Value *sp = roots->safepoint.sp;
         sp < roots->safepoint.thread->sp_base;
         sp++)
      trace_tagged_edge(trace_edge, gc_edge(sp), heap, trace_data);
  }
}

static inline void gc_trace_heap_roots(struct gc_heap_roots *roots,
                                       void (*trace_edge)(struct gc_edge edge,
                                                          struct gc_heap *heap,
                                                          void *trace_data),
                                       struct gc_heap *heap,
                                       void *trace_data) {
  if (roots)
    GC_CRASH();
}

static inline uintptr_t gc_object_forwarded_nonatomic(struct gc_ref ref) {
  Tagged *v = gc_ref_heap_object(ref);
  return tag_is_forwarded(v->tag) ? tag_forwarded_addr(v->tag) : 0;
}

static inline void gc_object_forward_nonatomic(struct gc_ref ref,
                                               struct gc_ref new_ref) {
  Tagged *old = gc_ref_heap_object(ref);
  old->tag = make_forwarded_tag(gc_ref_heap_object(new_ref));
}

static inline void gc_object_set_remembered(struct gc_ref ref) {
  tagged_set_remembered(gc_ref_heap_object(ref));
}

static inline int gc_object_is_remembered_nonatomic(struct gc_ref ref) {
  return tagged_remembered(gc_ref_heap_object(ref));
}

static inline void gc_object_clear_remembered_nonatomic(struct gc_ref ref) {
  return tagged_clear_remembered(gc_ref_heap_object(ref));
}

static inline struct gc_atomic_forward
gc_atomic_forward_begin(struct gc_ref ref) {
  Tagged *obj = gc_ref_heap_object(ref);
  uintptr_t observed = atomic_load_explicit(&obj->tag, memory_order_acquire);
  enum gc_forwarding_state state;
  if (observed == BUSY_TAG)
    state = GC_FORWARDING_STATE_BUSY;
  else if (tag_is_forwarded(observed))
    state = GC_FORWARDING_STATE_FORWARDED;
  else
    state = GC_FORWARDING_STATE_NOT_FORWARDED;
  return (struct gc_atomic_forward){ ref, observed, state };
}

static inline int
gc_atomic_forward_retry_busy(struct gc_atomic_forward *fwd) {
  GC_ASSERT(fwd->state == GC_FORWARDING_STATE_BUSY);
  Tagged *obj = gc_ref_heap_object(fwd->ref);
  uintptr_t observed = atomic_load_explicit(&obj->tag, memory_order_acquire);
  if (observed == BUSY_TAG)
    return 0;
  if (tag_is_forwarded(observed)) {
    fwd->state = GC_FORWARDING_STATE_FORWARDED;
    fwd->data = observed;
    return 1;
  }
  fwd->state = GC_FORWARDING_STATE_ABORTED;
  return 1;
}
  
static inline void
gc_atomic_forward_acquire(struct gc_atomic_forward *fwd) {
  GC_ASSERT(fwd->state == GC_FORWARDING_STATE_NOT_FORWARDED);
  Tagged *obj = gc_ref_heap_object(fwd->ref);
  if (atomic_compare_exchange_strong(&obj->tag, &fwd->data, BUSY_TAG))
    fwd->state = GC_FORWARDING_STATE_ACQUIRED;
  else if (fwd->data == BUSY_TAG)
    fwd->state = GC_FORWARDING_STATE_BUSY;
  else {
    GC_ASSERT(tag_is_forwarded(fwd->data));
    fwd->state = GC_FORWARDING_STATE_FORWARDED;
  }
}

static inline void
gc_atomic_forward_abort(struct gc_atomic_forward *fwd) {
  GC_ASSERT(fwd->state == GC_FORWARDING_STATE_ACQUIRED);
  Tagged *obj = gc_ref_heap_object(fwd->ref);
  atomic_store_explicit(&obj->tag, fwd->data, memory_order_release);
  fwd->state = GC_FORWARDING_STATE_ABORTED;
}

static inline void
gc_atomic_forward_commit(struct gc_atomic_forward *fwd, struct gc_ref new_ref) {
  GC_ASSERT(fwd->state == GC_FORWARDING_STATE_ACQUIRED);
  Tagged *obj = gc_ref_heap_object(fwd->ref);
  Tagged *new_obj = gc_ref_heap_object(new_ref);
  new_obj->tag = fwd->data;
  uintptr_t forwarded_tag = make_forwarded_tag(new_obj);
  atomic_store_explicit(&obj->tag, forwarded_tag, memory_order_release);
  fwd->state = GC_FORWARDING_STATE_FORWARDED;
}

static inline uintptr_t
gc_atomic_forward_address(struct gc_atomic_forward *fwd) {
  GC_ASSERT(fwd->state == GC_FORWARDING_STATE_FORWARDED);
  return tag_forwarded_addr(fwd->data);
}

#endif // WHIFFLE_EMBEDDER_H
