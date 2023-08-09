#ifndef GC_EMBEDDER_API_H
#define GC_EMBEDDER_API_H

#include <stddef.h>

#include "gc-config.h"
#include "gc-edge.h"
#include "gc-inline.h"
#include "gc-forwarding.h"

#ifndef GC_EMBEDDER_API
#define GC_EMBEDDER_API static
#endif

struct gc_mutator_roots;
struct gc_heap_roots;
struct gc_atomic_forward;
struct gc_heap;
struct gc_ephemeron;

GC_EMBEDDER_API inline int gc_is_valid_conservative_ref_displacement(uintptr_t displacement);

GC_EMBEDDER_API inline void gc_trace_object(struct gc_ref ref,
                                            void (*visit)(struct gc_edge edge,
                                                          struct gc_heap *heap,
                                                          void *visit_data),
                                            struct gc_heap *heap,
                                            void *trace_data,
                                            size_t *size) GC_ALWAYS_INLINE;

GC_EMBEDDER_API inline void gc_trace_mutator_roots(struct gc_mutator_roots *roots,
                                                   void (*trace_edge)(struct gc_edge edge,
                                                                      struct gc_heap *heap,
                                                                      void *trace_data),
                                                   struct gc_heap *heap,
                                                   void *trace_data);
GC_EMBEDDER_API inline void gc_trace_heap_roots(struct gc_heap_roots *roots,
                                                void (*trace_edge)(struct gc_edge edge,
                                                                   struct gc_heap *heap,
                                                                   void *trace_data),
                                                struct gc_heap *heap,
                                                void *trace_data);

// Some heap objects have space for a "remembered" bit, indicating they
// are in the remembered set.  Large or potentially large objects
// (e.g. a vector whose size is a run-time property) must have a
// remembered set bit.  Small objects may or may not have such a bit.
GC_EMBEDDER_API inline void gc_object_set_remembered(struct gc_ref ref);
GC_EMBEDDER_API inline int gc_object_is_remembered_nonatomic(struct gc_ref ref);
GC_EMBEDDER_API inline void gc_object_clear_remembered_nonatomic(struct gc_ref ref);

GC_EMBEDDER_API inline uintptr_t gc_object_forwarded_nonatomic(struct gc_ref ref);
GC_EMBEDDER_API inline void gc_object_forward_nonatomic(struct gc_ref ref,
                                                        struct gc_ref new_ref);

GC_EMBEDDER_API inline struct gc_atomic_forward gc_atomic_forward_begin(struct gc_ref ref);
GC_EMBEDDER_API inline void gc_atomic_forward_acquire(struct gc_atomic_forward *);
GC_EMBEDDER_API inline int gc_atomic_forward_retry_busy(struct gc_atomic_forward *);
GC_EMBEDDER_API inline void gc_atomic_forward_abort(struct gc_atomic_forward *);
GC_EMBEDDER_API inline void gc_atomic_forward_commit(struct gc_atomic_forward *,
                                                     struct gc_ref new_ref);
GC_EMBEDDER_API inline uintptr_t gc_atomic_forward_address(struct gc_atomic_forward *);


#endif // GC_EMBEDDER_API_H
