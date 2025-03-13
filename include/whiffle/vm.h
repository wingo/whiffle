#ifndef WHIFFLE_VM_H
#define WHIFFLE_VM_H

#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/time.h>

#include <gc-api.h>
#include <gc-basic-stats.h>
#include <gc-ephemeron.h>

#include "types.h"

static inline intptr_t fixnum_value(Value v) { return value_to_fixnum(v); }
static inline Value make_fixnum(intptr_t v) { return value_from_fixnum(v); }

struct vm_options {
  int print_stats;
  char *gc_options;
  uint64_t prng_seed;
};

struct vm_process {
  int *global_safepoint_flag_loc;
  struct vm_options options;
  struct gc_basic_stats stats;
};

static void vm_usage(FILE *f, char *arg0) {
  fprintf(f, "usage: %s [--print-stats] [--seed SEED] [--gc-options OPTIONS] ARG...\n",
          arg0);
}

static inline uint64_t splitmix64(uint64_t *seed) {
  // splitmix64 code taken from https://prng.di.unimi.it/splitmix64.c,
  // whose copyright notice is:
  //
  // Written in 2015 by Sebastiano Vigna (vigna@acm.org)
  //
  // To the extent possible under law, the author has dedicated all
  // copyright and related and neighboring rights to this software to
  // the public domain worldwide. This software is distributed without
  // any warranty.
  //
  // See <http://creativecommons.org/publicdomain/zero/1.0/>.

  uint64_t z = (*seed += UINT64_C(0x9e3779b97f4a7c15));
  z = (z ^ (z >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
  z = (z ^ (z >> 27)) * UINT64_C(0x94d049bb133111eb);
  return z ^ (z >> 31);
}

static inline void vm_init_random(struct VM vm, uint64_t seed) {
  vm.thread->prng_state.s[0] = splitmix64(&seed);
  vm.thread->prng_state.s[1] = splitmix64(&seed);
  vm.thread->prng_state.s[2] = splitmix64(&seed);
  vm.thread->prng_state.s[3] = splitmix64(&seed);
}

static void vm_parse_options(struct vm_options *options, int *argc_p,
                             char ***argv_p) {
  int argc = *argc_p;
  char **argv = *argv_p;
  if (argc == 0) {
    vm_usage(stderr, "[missing argv[0]]");
    exit(1);
  }
  char *arg0 = argv[0];
  argc--, argv++;
  options->gc_options = getenv("GC_OPTIONS");
  for (; argc && *argv[0] == '-'; argc--, argv++) {
    char *arg = argv[0];
    if (argc > 1 && !strcmp(arg, "--gc-options")) {
      options->gc_options = argv[1];
      argc--, argv++;
    } else if (argc > 1 && !strcmp(arg, "--seed")) {
      options->prng_seed = atoll(argv[1]);
      argc--, argv++;
    } else if (!strcmp(arg, "--print-stats")) {
      options->print_stats = 1;
    } else if (!strcmp(arg, "--help") || !strcmp(arg, "-h")) {
      vm_usage(stdout, arg0);
      exit(0);
    } else {
      vm_usage(stderr, arg0);
      exit(1);
    }
  }
  *argc_p = argc;
  *argv_p = argv;
}

static int vm_should_safepoint(struct vm_process *process,
                               Thread *thread) {
  switch (gc_cooperative_safepoint_kind()) {
  case GC_COOPERATIVE_SAFEPOINT_NONE:
    return 0;
  case GC_COOPERATIVE_SAFEPOINT_HEAP_FLAG:
    return atomic_load_explicit(process->global_safepoint_flag_loc,
                                memory_order_relaxed);
  case GC_COOPERATIVE_SAFEPOINT_MUTATOR_FLAG:
    return atomic_load_explicit(gc_safepoint_flag_loc(thread->mut),
                                memory_order_relaxed);
  default:
    GC_CRASH();
  }
}

static VM vm_prepare_process(struct vm_process *process,
                             Thread *thread,
                             int *argc_p, char ***argv_p) {
  memset(process, 0, sizeof(*process));
  vm_parse_options(&process->options, argc_p, argv_p);

  size_t bytes = 2 * 1024 * 1024;
  void *mem = mmap(NULL, bytes, PROT_READ|PROT_WRITE,
                   MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED) {
    perror("allocating thread stack failed");
    exit(1);
  }
  thread->sp_base = (Value*)(((char *)mem) + bytes);
  thread->sp_limit = mem;
  struct gc_options *options = gc_allocate_options();
  char *options_str = process->options.gc_options;
  if (options_str) {
    if (!gc_options_parse_and_set_many(options, options_str)) {
      fprintf(stderr, "failed to set options: %s\n", options_str);
      exit(1);
    }
  }
  if (!gc_init(options, NULL, &thread->heap, &thread->mut,
               GC_BASIC_STATS, &process->stats))
    GC_CRASH();

  if (gc_cooperative_safepoint_kind() == GC_COOPERATIVE_SAFEPOINT_HEAP_FLAG)
    process->global_safepoint_flag_loc = gc_safepoint_flag_loc(thread->mut);

  gc_mutator_set_roots(thread->mut, &thread->roots);
  VM vm = (VM){thread, thread->sp_base};
  thread->roots.safepoint = vm;

  memset(&thread->extern_space, 0, sizeof(thread->extern_space));
  gc_heap_set_extern_space(thread->heap, &thread->extern_space);

  vm_init_random(vm, process->options.prng_seed);

  return vm;
}

static int vm_finish_process(Thread *thread, struct vm_process *process) {
  gc_basic_stats_finish(&process->stats);
  if (process->options.print_stats) {
    gc_basic_stats_print(&process->stats, stdout);
    fprintf(stdout, "#(%f %f %f %f %" PRIu64 " %" PRIu64 " %f %f %f)\n",
            (double) process->stats.elapsed_mutator_usec * 1e-6,
            (double) process->stats.elapsed_collector_usec * 1e-6,
            (double) process->stats.cpu_mutator_usec * 1e-6,
            (double) process->stats.cpu_collector_usec * 1e-6,
            process->stats.major_collection_count,
            process->stats.minor_collection_count,
            gc_latency_median(&process->stats.pause_times) * 1e-6,
            gc_latency_percentile(&process->stats.pause_times, 0.95) * 1e-6,
            gc_latency_max(&process->stats.pause_times) * 1e-6);
  }
  return 0;
}

static inline VM vm_trim(VM vm, size_t n) {
  if (GC_DEBUG)
    memset(vm.sp, 0, sizeof(Value) * n);
  return ((VM){vm.thread, vm.sp + n});
}

static inline void vm_record_preemptive_safepoint(VM vm) {
  if (gc_safepoint_mechanism() == GC_SAFEPOINT_MECHANISM_SIGNAL)
    vm.thread->roots.safepoint = vm;
}
static inline void vm_record_cooperative_safepoint(VM vm) {
  if (gc_safepoint_mechanism() == GC_SAFEPOINT_MECHANISM_COOPERATIVE)
    vm.thread->roots.safepoint = vm;
}


#define VM_CRASH(reason)                                                \
  do {                                                                  \
    fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, reason);         \
    abort();                                                            \
  } while (0)

#define VM_CHECK(x)                                                     \
  do {                                                                  \
    if (GC_UNLIKELY(!(x))) {                                            \
      VM_CRASH("assertion failed: " #x);                                \
    }                                                                   \
  } while (0)

static inline void vm_check_stack(VM vm, size_t slots) {
  VM_CHECK(vm.sp - vm.thread->sp_limit >= slots);
}

static inline VM vm_expand_stack(VM vm, size_t slots) {
  vm.sp -= slots;
  if (gc_safepoint_mechanism() == GC_SAFEPOINT_MECHANISM_SIGNAL) {
    memset(vm.sp, 0, sizeof(Value) * slots);
    vm_record_preemptive_safepoint(vm);
  }
  return vm;
}

static inline Value vm_make_closure(VM vm, Code code, size_t nfree) {
  size_t bytes = sizeof(Closure) + nfree * sizeof(Value);
  Closure *ret = gc_allocate_fast(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  }
  tagged_set_payload(&ret->tag, CLOSURE_TAG, nfree);
  ret->code = code;

  // Because one might allocate multiple closures then initialize them
  // all together in a <fix>, ensure the fields hold sensible values.
  for (size_t i = 0; i < nfree; i++)
    ret->free_vars[i] = IMMEDIATE_FALSE;

  return value_from_heap_object(ret);
}

static inline void vm_closure_init(VM vm, Value closure, size_t idx, Value val) {
  Closure *c = value_to_heap_object(closure);
  size_t size = tagged_payload(&c->tag);
  gc_write_barrier(vm.thread->mut, gc_ref_from_heap_object(c),
                   sizeof(*c) + sizeof(Value)*size,
                   gc_edge(&c->free_vars[idx]), gc_ref(val.payload));
  c->free_vars[idx] = val;
}

static inline Value vm_closure_ref(Value closure, size_t idx) {
  Closure *c = value_to_heap_object(closure);
  VM_CHECK(idx < tagged_payload(&c->tag));
  return c->free_vars[idx];
}

static inline int is_closure(Value v) {
  return is_heap_object(v) &&
    tagged_kind(value_to_heap_object(v)) == CLOSURE_TAG;
}
static inline Code vm_closure_code(Value closure) {
  VM_CHECK(is_closure(closure));
  Closure *c = value_to_heap_object(closure);
  return c->code;
}

static inline Value vm_box(VM vm, Value *val_loc) {
  size_t bytes = sizeof(Box);
  Box *ret = gc_allocate_fast(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  }
  tagged_set_payload(&ret->tag, BOX_TAG, 0);
  ret->val = *val_loc;
  return value_from_heap_object(ret);
}

static inline int is_box(Value v) {
  return is_heap_object(v) &&
    tagged_kind(value_to_heap_object(v)) == BOX_TAG;
}
static inline int vm_is_box(Value v) { return is_box(v); }

static inline void vm_box_set(VM vm, Value box, Value val) {
  VM_CHECK(is_box(box));
  Box *b = value_to_heap_object(box);
  gc_write_barrier(vm.thread->mut, gc_ref_from_heap_object(b), sizeof(*b),
                   gc_edge(&b->val), gc_ref(val.payload));
  b->val = val;
}

static inline Value vm_box_ref(Value box) {
  VM_CHECK(is_box(box));
  Box *b = value_to_heap_object(box);
  return b->val;
}

static inline Value vm_add(Value a, Value b) {
  VM_CHECK(is_fixnum(a));
  VM_CHECK(is_fixnum(b));
  return make_fixnum(fixnum_value(a) + fixnum_value(b));
}

static inline Value vm_sub(Value a, Value b) {
  VM_CHECK(is_fixnum(a));
  VM_CHECK(is_fixnum(b));
  return make_fixnum(fixnum_value(a) - fixnum_value(b));
}

static inline Value vm_mul(Value a, Value b) {
  VM_CHECK(is_fixnum(a));
  VM_CHECK(is_fixnum(b));
  return make_fixnum(fixnum_value(a) * fixnum_value(b));
}

static inline Value vm_quo(Value a, Value b) {
  VM_CHECK(is_fixnum(a));
  VM_CHECK(is_fixnum(b));
  VM_CHECK(fixnum_value(b) != 0);
  return make_fixnum(fixnum_value(a) / fixnum_value(b));
}

static inline Value vm_rem(Value a, Value b) {
  VM_CHECK(is_fixnum(a));
  VM_CHECK(is_fixnum(b));
  VM_CHECK(fixnum_value(b) != 0);
  return make_fixnum(fixnum_value(a) % fixnum_value(b));
}

static inline Value vm_cons(VM vm, Value *car_loc, Value *cdr_loc) {
  size_t bytes = sizeof(Pair);
  Pair *ret = gc_allocate_fast(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  }
  tagged_set_value(&ret->tag, PAIR_TAG, *car_loc);
  ret->cdr = *cdr_loc;
  return value_from_heap_object(ret);
}

static inline int is_pair(Value x) {
  return is_heap_object(x) && tagged_is_pair(value_to_heap_object(x));
}

static inline Value vm_car(Value pair) {
  VM_CHECK(is_pair(pair));
  Pair *p = value_to_heap_object(pair);
  return tagged_value(&p->tag);
}

static inline Value vm_cdr(Value pair) {
  VM_CHECK(is_pair(pair));
  Pair *p = value_to_heap_object(pair);
  return p->cdr;
}

static inline void vm_set_car(VM vm, Value pair, Value val) {
  VM_CHECK(is_pair(pair));
  Pair *p = value_to_heap_object(pair);
  gc_write_barrier(vm.thread->mut, gc_ref_from_heap_object(p), sizeof(*p),
                   gc_edge(&p->tag), gc_ref(val.payload));
  tagged_set_value(&p->tag, PAIR_TAG, val);
}

static inline void vm_set_cdr(VM vm, Value pair, Value val) {
  VM_CHECK(is_pair(pair));
  Pair *p = value_to_heap_object(pair);
  gc_write_barrier(vm.thread->mut, gc_ref_from_heap_object(p), sizeof(*p),
                   gc_edge(&p->cdr), gc_ref(val.payload));
  p->cdr = val;
}

static inline Value vm_make_vector(VM vm, Value size, Value *init_loc) {
  VM_CHECK(is_fixnum(size));
  intptr_t c_size = fixnum_value(size);
  VM_CHECK(0 <= c_size && c_size <= (((uintptr_t)-1) >> 8));
  size_t bytes = sizeof(Vector) + c_size * sizeof(Value);
  Vector *ret = gc_allocate_fast(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  }
  tagged_set_payload(&ret->tag, VECTOR_TAG, c_size);

  // Because one might allocate multiple closures then initialize them
  // all together in a <fix>, ensure the fields hold sensible values.
  Value init = *init_loc;
  for (size_t i = 0; i < c_size; i++)
    ret->vals[i] = init;

  return value_from_heap_object(ret);
}

static inline Value vm_allocate_vector(VM vm, size_t size) {
  VM_CHECK(size <= (((uintptr_t)-1) >> 8));
  size_t bytes = sizeof(Vector) + size * sizeof(Value);
  Vector *ret = gc_allocate_fast(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  }
  tagged_set_payload(&ret->tag, VECTOR_TAG, size);
  return value_from_heap_object(ret);
}

static inline int is_vector(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == VECTOR_TAG;
}

static inline Value vm_vector_length(Value vector) {
  VM_CHECK(is_vector(vector));
  Vector *v = value_to_heap_object(vector);
  return make_fixnum(tagged_payload(&v->tag));
}

static inline Value vm_vector_ref(Value vector, Value idx) {
  VM_CHECK(is_vector(vector));
  VM_CHECK(is_fixnum(idx));
  Vector *v = value_to_heap_object(vector);
  size_t size = tagged_payload(&v->tag);
  intptr_t c_idx = fixnum_value(idx);
  VM_CHECK(0 <= c_idx && c_idx < size);
  return v->vals[c_idx];
}

static inline void vm_vector_set(VM vm, Value vector, Value idx, Value val) {
  VM_CHECK(is_vector(vector));
  VM_CHECK(is_fixnum(idx));
  Vector *v = value_to_heap_object(vector);
  size_t size = tagged_payload(&v->tag);
  intptr_t c_idx = fixnum_value(idx);
  VM_CHECK(0 <= c_idx && c_idx < size);
  gc_write_barrier(vm.thread->mut, gc_ref_from_heap_object(v),
                   sizeof(*v) + sizeof(Value)*size,
                   gc_edge(&v->vals[c_idx]), gc_ref(val.payload));
  v->vals[c_idx] = val;
}

static inline void vm_vector_init(Value vector, size_t idx, Value val) {
  Vector *v = value_to_heap_object(vector);
  v->vals[idx] = val;
}

static inline int is_string(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == STRING_TAG;
}

static inline Value vm_string_to_vector(Value str) {
  VM_CHECK(is_string(str));
  String *s = value_to_heap_object(str);
  return value_from_heap_object(s->chars);
}

static inline Value vm_vector_to_string(VM vm, Value *v) {
  {
    VM_CHECK(is_vector(*v));
    Vector *vec = value_to_heap_object(*v);
    size_t len = tagged_payload(&vec->tag);
    for (size_t i = 0; i < len; i++)
      VM_CHECK(is_char(vec->vals[i]));
  }
  size_t bytes = sizeof(String);
  String *ret = gc_allocate_fast(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  }
  tagged_set_payload(&ret->tag, STRING_TAG, 0);
  ret->chars = value_to_heap_object(*v);
  return value_from_heap_object(ret);
}

static inline int is_symbol(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == SYMBOL_TAG;
}

static inline Value vm_symbol_to_string(Value sym) {
  VM_CHECK(is_symbol(sym));
  Symbol *s = value_to_heap_object(sym);
  return value_from_heap_object(s->str);
}

static inline Value vm_string_to_symbol(VM vm, Value *str) {
  VM_CHECK(is_string(*str));
  size_t bytes = sizeof(Symbol);
  Symbol *ret = gc_allocate_fast(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  }
  tagged_set_payload(&ret->tag, SYMBOL_TAG, 0);
  ret->str = value_to_heap_object(*str);
  return value_from_heap_object(ret);
}

static inline Value vm_make_bytevector(VM vm, Value *size, Value *init) {
  VM_CHECK(is_fixnum(*size));
  intptr_t c_size = fixnum_value(*size);
  intptr_t c_init = fixnum_value(*init);
  VM_CHECK(0 <= c_size && c_size <= (((uintptr_t)-1) >> 8));
  VM_CHECK(-128 <= c_init && c_init <= 255);
  size_t bytes = sizeof(Bytevector) + c_size * sizeof(uint8_t);
  Bytevector *ret = gc_allocate_fast(vm.thread->mut, bytes,
                                     GC_ALLOCATION_TAGGED_POINTERLESS);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes,
                           GC_ALLOCATION_TAGGED_POINTERLESS);
  }
  tagged_set_payload(&ret->tag, BYTEVECTOR_TAG, c_size);
  memset(ret->vals, c_init, c_size);

  return value_from_heap_object(ret);
}

static inline int is_bytevector(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == BYTEVECTOR_TAG;
}

static inline Value vm_bytevector_length(Value bytevector) {
  VM_CHECK(is_bytevector(bytevector));
  Bytevector *v = value_to_heap_object(bytevector);
  return make_fixnum(tagged_payload(&v->tag));
}

static inline Value vm_bytevector_u8_ref(Value bytevector, Value idx) {
  VM_CHECK(is_bytevector(bytevector));
  VM_CHECK(is_fixnum(idx));
  Bytevector *v = value_to_heap_object(bytevector);
  size_t size = tagged_payload(&v->tag);
  intptr_t c_idx = fixnum_value(idx);
  VM_CHECK(0 <= c_idx && c_idx < size);
  return value_from_fixnum(v->vals[c_idx]);
}

static inline void vm_bytevector_u8_set(Value bytevector, Value idx, Value val) {
  VM_CHECK(is_bytevector(bytevector));
  VM_CHECK(is_fixnum(idx));
  VM_CHECK(is_fixnum(val));
  Bytevector *v = value_to_heap_object(bytevector);
  size_t size = tagged_payload(&v->tag);
  intptr_t c_idx = fixnum_value(idx);
  intptr_t c_val = fixnum_value(val);
  VM_CHECK(0 <= c_idx && c_idx < size);
  VM_CHECK(-128 <= c_val && c_val <= 255);
  v->vals[c_idx] = c_val;
}

static inline Value vm_make_ephemeron(VM vm, Value *key_loc, Value *value_loc) {
  VM_CHECK(is_heap_object(*key_loc));
  vm_record_cooperative_safepoint(vm);
  Ephemeron *ret = gc_allocate_ephemeron(vm.thread->mut);
  tagged_set_payload((Tagged*)ret, EPHEMERON_TAG, 0);
  gc_ephemeron_init(vm.thread->mut, ret,
                    gc_ref_from_heap_object(value_to_heap_object(*key_loc)),
                    gc_ref(value_loc->payload));
  return value_from_heap_object(ret);
}

static inline int is_ephemeron(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == EPHEMERON_TAG;
}

static inline Value vm_ephemeron_key(Value ephemeron) {
  VM_CHECK(is_ephemeron(ephemeron));
  Ephemeron *e = value_to_heap_object(ephemeron);
  struct gc_ref k = gc_ephemeron_key(e);
  return gc_ref_is_heap_object(k)
    ? value_from_heap_object(gc_ref_heap_object(k)) : IMMEDIATE_FALSE;
}

static inline Value vm_ephemeron_value(Value ephemeron) {
  VM_CHECK(is_ephemeron(ephemeron));
  Ephemeron *e = value_to_heap_object(ephemeron);
  struct gc_ref v = gc_ephemeron_value(e);
  return gc_ref_is_heap_object(v)
    ? value_from_heap_object(gc_ref_heap_object(v)) : IMMEDIATE_FALSE;
}

static inline Value vm_ephemeron_next(Value ephemeron) {
  VM_CHECK(is_ephemeron(ephemeron));
  Ephemeron *e = value_to_heap_object(ephemeron);
  Ephemeron *next = gc_ephemeron_chain_next(e);
  return next ? value_from_heap_object(next) : IMMEDIATE_FALSE;
}

static inline void vm_ephemeron_kill(Value ephemeron) {
  VM_CHECK(is_ephemeron(ephemeron));
  Ephemeron *e = value_to_heap_object(ephemeron);
  gc_ephemeron_mark_dead(e);
}

static inline Value vm_make_ephemeron_table(VM vm, Value *size_loc) {
  VM_CHECK(is_fixnum(*size_loc));
  intptr_t c_size = fixnum_value(*size_loc);
  VM_CHECK(0 <= c_size && c_size <= (((uintptr_t)-1) >> 8));
  size_t bytes = sizeof(EphemeronTable) + c_size * sizeof(Value);
  EphemeronTable *ret = gc_allocate_fast(vm.thread->mut, bytes,
                                         GC_ALLOCATION_TAGGED);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes, GC_ALLOCATION_TAGGED);
  }
  tagged_set_payload(&ret->tag, EPHEMERON_TABLE_TAG, c_size);
  // No initialization of fields, as we rely on collector to provide
  // zero-initialized memory.
  return value_from_heap_object(ret);
}

static inline int is_ephemeron_table(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == EPHEMERON_TABLE_TAG;
}

static inline Value vm_ephemeron_table_length(Value table) {
  VM_CHECK(is_ephemeron_table(table));
  EphemeronTable *t = value_to_heap_object(table);
  return value_from_fixnum(tagged_payload(&t->tag));
}

static inline Value vm_ephemeron_table_ref(Value table, Value idx) {
  VM_CHECK(is_ephemeron_table(table));
  EphemeronTable *t = value_to_heap_object(table);
  VM_CHECK(is_fixnum(idx));
  size_t size = tagged_payload(&t->tag);
  intptr_t c_idx = fixnum_value(idx);
  VM_CHECK(0 <= c_idx && c_idx < size);
  Ephemeron *e = gc_ephemeron_chain_head(&t->vals[c_idx]);
  return e ? value_from_heap_object(e) : IMMEDIATE_FALSE;
}

static inline void vm_ephemeron_table_push(VM vm, Value table, Value idx,
                                           Value ephemeron) {
  VM_CHECK(is_ephemeron_table(table));
  VM_CHECK(is_ephemeron(ephemeron));
  EphemeronTable *t = value_to_heap_object(table);
  Ephemeron *e = value_to_heap_object(ephemeron);
  VM_CHECK(is_fixnum(idx));
  size_t size = tagged_payload(&t->tag);
  intptr_t c_idx = fixnum_value(idx);
  VM_CHECK(0 <= c_idx && c_idx < size);
  gc_write_barrier(vm.thread->mut, gc_ref_from_heap_object(t),
                   sizeof(*t) + sizeof(Value)*size,
                   gc_edge(&t->vals[c_idx]), gc_ref_from_heap_object(e));
  gc_ephemeron_chain_push(&t->vals[c_idx], e);
}

static inline Value vm_char_to_integer(Value x) {
  return value_from_fixnum(value_to_char(x));
}

static inline Value vm_integer_to_char(Value x) {
  intptr_t i = value_to_fixnum(x);
  VM_CHECK(0 <= i && i < (1 << 21));
  return value_from_char(i);
}

static inline void vm_write_char(Value ch) {
  fputc(value_to_char(ch), stdout);
}

static inline int vm_is_false(Value val) {
  return val.payload == IMMEDIATE_FALSE.payload;
}

static inline int vm_is_fixnum(Value val) {
  return is_fixnum(val);
}

static inline int vm_is_pair(Value val) {
  return is_pair(val);
}

static inline int vm_is_vector(Value val) {
  return is_vector(val);
}

static inline int vm_is_string(Value val) {
  return is_string(val);
}

static inline int vm_is_symbol(Value val) {
  return is_symbol(val);
}

static inline int vm_is_bytevector(Value val) {
  return is_bytevector(val);
}

static inline int vm_is_ephemeron(Value val) {
  return is_ephemeron(val);
}

static inline int vm_is_ephemeron_table(Value val) {
  return is_ephemeron_table(val);
}

static inline int vm_is_char(Value val) {
  return is_char(val);
}

static inline int vm_is_eq(Value a, Value b) {
  return a.payload == b.payload;
}

static inline int vm_is_less(Value a, Value b) {
  VM_CHECK(is_fixnum(a));
  VM_CHECK(is_fixnum(b));
  return fixnum_value(a) < fixnum_value(b);
}

static inline int vm_is_numerically_eq(Value a, Value b) {
  VM_CHECK(is_fixnum(a));
  VM_CHECK(is_fixnum(b));
  return fixnum_value(a) == fixnum_value(b);
}

static inline Value vm_parse_value(VM vm, const char *val) {
  char *endptr;
  long long ival;

  errno = 0;
  ival = strtoll(val, &endptr, 0);
  if (errno) {
    perror("failed to parse arg");
    exit(1);
  }
  if (endptr == val || *endptr != '\0') {
    fprintf(stderr, "failed to parse arg: %s\n", val);
    exit(1);
  }
  if (ival <= FIXNUM_MIN || FIXNUM_MAX <= ival) {
    fprintf(stderr, "value out of range: %s\n", val);
    exit(1);
  }
  return value_from_fixnum(ival);
}

static inline void vm_print_value(Value val) {
  if (is_fixnum(val)) {
    intptr_t ival = fixnum_value(val);
    fprintf(stdout, "%lld\n", (long long)ival);
  } else if (is_string(val)) {
    String *s = value_to_heap_object(val);
    Vector *v = s->chars;
    fputc('"', stdout);
    for (size_t i = 0; i < tagged_payload(&v->tag); i++)
      fputc(value_to_char(v->vals[i]), stdout);
    fputc('"', stdout);
    fputc('\n', stdout);
  } else {
    VM_CRASH("unexpected value");
  }
}

static inline int is_thread(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == THREAD_TAG;
}

static void* vm_thread_transition_to_running(void *data) {
  Thread *thread = data;
  pthread_mutex_lock(&thread->lock);
  VM_CHECK(thread->state == THREAD_SPAWNING);
  thread->state = THREAD_WAITING;
  pthread_cond_signal(&thread->cond);
  do
    pthread_cond_wait(&thread->cond, &thread->lock);
  while (thread->state == THREAD_WAITING);
  VM_CHECK(thread->state == THREAD_RUNNING);
  pthread_mutex_unlock(&thread->lock);
  return NULL;
}

static void* vm_thread_wait_for_stopping(void *data) {
  Thread *thread = data;
  pthread_mutex_lock(&thread->lock);
  while (thread->state < THREAD_STOPPING)
    pthread_cond_wait(&thread->cond, &thread->lock);
  VM_CHECK(thread->state == THREAD_STOPPING);
  // Keep lock.
  return NULL;
}

static void* vm_thread_transition_to_stopped(void *data) {
  Thread *thread = data;
  pthread_mutex_lock(&thread->lock);
  VM_CHECK(thread->state == THREAD_RUNNING);
  thread->state = THREAD_STOPPING;
  pthread_cond_signal(&thread->cond);
  while (thread->state == THREAD_STOPPING)
    pthread_cond_wait(&thread->cond, &thread->lock);
  VM_CHECK(thread->state == THREAD_STOPPED);
  pthread_mutex_unlock(&thread->lock);
  return NULL;
}

struct vm_spawn_thread_data { struct gc_heap *heap; Thread *thread; };

static void* vm_thread_proc_inner(struct gc_stack_addr *stack_base,
                                  void *data) {
  struct vm_spawn_thread_data *spawn_data = data;
  struct gc_heap *heap = spawn_data->heap;
  Thread *thread = spawn_data->thread;
  size_t bytes = 2 * 1024 * 1024;
  void *mem = mmap(NULL, bytes, PROT_READ|PROT_WRITE,
                   MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED) {
    perror("allocating thread stack failed");
    GC_CRASH();
  }
  thread->sp_base = (Value*)(((char *)mem) + bytes);
  thread->sp_limit = mem;

  thread->mut = gc_init_for_thread(stack_base, heap);

  // Prep the VM to have one stack slot, holding #f.
  Value *sp = thread->sp_base - 1;
  VM vm = (VM){thread, sp};
  vm.sp[0] = IMMEDIATE_FALSE;
  thread->roots.safepoint = vm;
  gc_mutator_set_roots(thread->mut, &thread->roots);

  // Now that we mark our stack and have one reserved stack slot,
  // receive the thunk into the stack slot.
  gc_call_without_gc(thread->mut,
                     vm_thread_transition_to_running,
                     thread);

  // Run the thunk.
  vm = vm_closure_code(vm.sp[0])(vm, 1);
  // Must have one value live: the return value.
  VM_CHECK(vm.sp == thread->sp_base - 1);
  thread->roots.safepoint = vm;

  // RDV with joiner to send result back.
  gc_call_without_gc(thread->mut,
                     vm_thread_transition_to_stopped,
                     thread);

  gc_finish_for_thread(thread->mut);
  munmap(mem, bytes);
  free(thread);
  return NULL;
}

static void* vm_thread_proc(void *data) {
  return gc_call_with_stack_addr(vm_thread_proc_inner, data);
}

static void* vm_spawn_thread_without_gc(void *data) {
  Thread *spawning_thread = data;
  struct gc_heap *heap = spawning_thread->heap;

  Thread *thread = calloc(1, sizeof(Thread));
  pthread_mutex_init(&thread->lock, NULL);
  pthread_cond_init(&thread->cond, NULL);
  thread->state = THREAD_SPAWNING;

  // Inherit deterministic PRNG state.
  memcpy(&thread->prng_state, &spawning_thread->prng_state,
         sizeof(thread->prng_state));

  struct vm_spawn_thread_data spawn_data = { heap, thread };
  pthread_mutex_lock(&thread->lock);
  pthread_create(&thread->tid, NULL, vm_thread_proc, &spawn_data);
  while (thread->state == THREAD_SPAWNING)
    pthread_cond_wait(&thread->cond, &thread->lock);

  VM_CHECK(thread->state == THREAD_WAITING);

  return thread;
}

static inline Value vm_spawn_thread(struct VM vm, Value *thunk) {
  VM_CHECK(is_closure(*thunk));

  vm_record_cooperative_safepoint(vm);

  Thread *thread = gc_call_without_gc(vm.thread->mut,
                                      vm_spawn_thread_without_gc,
                                      vm.thread);

  thread->sp_base[-1] = *thunk;
  thread->state = THREAD_RUNNING;
  pthread_cond_signal(&thread->cond);
  pthread_mutex_unlock(&thread->lock);

  ThreadHandle *handle = gc_allocate(vm.thread->mut, sizeof(ThreadHandle),
                                     GC_ALLOCATION_TAGGED);
  tagged_set_payload(&handle->tag, THREAD_TAG, 0);
  handle->thread = thread;
  return value_from_heap_object(handle);
}

static inline Value vm_join_thread(struct VM vm, Value *handle) {
  VM_CHECK(is_thread(*handle));
  ThreadHandle *th = value_to_heap_object(*handle);
  Thread *thread = th->thread;

  vm_record_cooperative_safepoint(vm);
  gc_call_without_gc(vm.thread->mut, vm_thread_wait_for_stopping, thread);

  Value ret = thread->sp_base[-1];
  thread->state = THREAD_STOPPED;
  pthread_cond_signal(&thread->cond);
  pthread_mutex_unlock(&thread->lock);

  return ret;
}

static inline Value vm_current_microseconds(void) {
  struct timeval t;
  if (gettimeofday(&t, NULL) == -1) VM_CRASH("gettimeofday failed");
  unsigned long usec = t.tv_sec * 1000 * 1000 + t.tv_usec;
  VM_CHECK(usec <= (unsigned long)FIXNUM_MAX);
  return value_from_fixnum(usec);
}

static inline void vm_die(void) {
  fflush(stdout);
  VM_CRASH("die");
}

static inline void vm_wrong_num_args(size_t expected, size_t actual) {
  fflush(stdout);
  fprintf(stderr, "wrong number of args to procedure (expected %zu, got %zu)\n",
          expected, actual);
  exit(1);
}

static inline Value vm_gc_collect(struct VM vm) {
  gc_collect(vm.thread->mut, GC_COLLECTION_MAJOR);
  return IMMEDIATE_TRUE;
}


static inline uint64_t rotl(const uint64_t x, int k) {
  return (x << k) | (x >> (64 - k));
}
static inline uint64_t xoshiro256ss(uint64_t *s) {
  // xoshiro256** code taken from
  // https://prng.di.unimi.it/xoshiro256starstar.c, which has the
  // following copyright notice:
  //
  // Written in 2018 by David Blackman and Sebastiano Vigna
  // (vigna@acm.org)
  //
  // To the extent possible under law, the author has dedicated all
  // copyright and related and neighboring rights to this software to
  // the public domain worldwide. This software is distributed without
  // any warranty.
  //
  // See <http://creativecommons.org/publicdomain/zero/1.0/>.
  uint64_t result = rotl(s[1] * 5, 7) * 9;

  uint64_t t = s[1] << 17;
  s[2] ^= s[0];
  s[3] ^= s[1];
  s[1] ^= s[2];
  s[0] ^= s[3];
  s[2] ^= t;
  s[3] = rotl(s[3], 45);

  return result;
}

static inline uint64_t thread_random64(Thread *thread) {
  return xoshiro256ss(thread->prng_state.s);
}

static inline Value thread_random_fixnum(Thread *thread) {
  return value_from_fixnum(thread_random64(thread));
}

#endif // WHIFFLE_VM_H
