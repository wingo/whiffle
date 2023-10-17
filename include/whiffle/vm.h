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
};

struct vm_process {
  struct vm_options options;
  struct gc_basic_stats stats;
};

static void vm_usage(FILE *f, char *arg0) {
  fprintf(f, "usage: %s [--print-stats] [--gc-options OPTIONS] ARG...\n",
          arg0);
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

  gc_mutator_set_roots(thread->mut, &thread->roots);
  VM vm = (VM){thread, thread->sp_base};
  thread->roots.safepoint = vm;

  memset(&thread->extern_space, 0, sizeof(thread->extern_space));
  gc_heap_set_extern_space(thread->heap, &thread->extern_space);

  return vm;
}

static int vm_finish_process(Thread *thread, struct vm_process *process) {
  gc_basic_stats_finish(&process->stats);
  if (process->options.print_stats)
    // TODO: Print a Scheme datum.
    gc_basic_stats_print(&process->stats, stdout);
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
  Closure *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes);
  }
  tagged_set_payload(&ret->tag, CLOSURE_TAG, nfree);
  ret->code = code;

  // Because one might allocate multiple closures then initialize them
  // all together in a <fix>, ensure the fields hold sensible values.
  for (size_t i = 0; i < nfree; i++)
    ret->free_vars[i] = IMMEDIATE_FALSE;

  return value_from_heap_object(ret);
}

static inline void vm_closure_init(Value closure, size_t idx, Value val) {
  Closure *c = value_to_heap_object(closure);
  c->free_vars[idx] = val;
}

static inline Value vm_closure_ref(Value closure, size_t idx) {
  Closure *c = value_to_heap_object(closure);
  if (idx >= tagged_payload(&c->tag)) abort();
  return c->free_vars[idx];
}

static inline int is_closure(Value v) {
  return is_heap_object(v) &&
    tagged_kind(value_to_heap_object(v)) == CLOSURE_TAG;
}
static inline Code vm_closure_code(Value closure) {
  if (!is_closure(closure)) abort();
  Closure *c = value_to_heap_object(closure);
  return c->code;
}

static inline Value vm_box(VM vm, Value *val_loc) {
  size_t bytes = sizeof(Box);
  Box *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes);
  }
  tagged_set_payload(&ret->tag, BOX_TAG, 0);
  ret->val = *val_loc;
  return value_from_heap_object(ret);
}

static inline int is_box(Value v) {
  return is_heap_object(v) &&
    tagged_kind(value_to_heap_object(v)) == BOX_TAG;
}

static inline void vm_box_set(Value box, Value val) {
  if (!is_box(box)) abort();
  Box *b = value_to_heap_object(box);
  gc_write_barrier(gc_ref_from_heap_object(b), sizeof(*b),
                   gc_edge(&b->val), gc_ref(val.payload));
  b->val = val;
}

static inline Value vm_box_ref(Value box) {
  if (!is_box(box)) abort();
  Box *b = value_to_heap_object(box);
  return b->val;
}

static inline Value vm_add(Value a, Value b) {
  if (!is_fixnum(a)) abort();
  if (!is_fixnum(b)) abort();
  return make_fixnum(fixnum_value(a) + fixnum_value(b));
}

static inline Value vm_sub(Value a, Value b) {
  if (!is_fixnum(a)) abort();
  if (!is_fixnum(b)) abort();
  return make_fixnum(fixnum_value(a) - fixnum_value(b));
}

static inline Value vm_mul(Value a, Value b) {
  if (!is_fixnum(a)) abort();
  if (!is_fixnum(b)) abort();
  return make_fixnum(fixnum_value(a) * fixnum_value(b));
}

static inline Value vm_quo(Value a, Value b) {
  if (!is_fixnum(a)) abort();
  if (!is_fixnum(b)) abort();
  if (fixnum_value(b) == 0) abort();
  return make_fixnum(fixnum_value(a) / fixnum_value(b));
}

static inline Value vm_rem(Value a, Value b) {
  if (!is_fixnum(a)) abort();
  if (!is_fixnum(b)) abort();
  if (fixnum_value(b) == 0) abort();
  return make_fixnum(fixnum_value(a) % fixnum_value(b));
}

static inline Value vm_cons(VM vm, Value *car_loc, Value *cdr_loc) {
  size_t bytes = sizeof(Pair);
  Pair *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes);
  }
  tagged_set_value(&ret->tag, PAIR_TAG, *car_loc);
  ret->cdr = *cdr_loc;
  return value_from_heap_object(ret);
}

static inline int is_pair(Value x) {
  return is_heap_object(x) && tagged_is_pair(value_to_heap_object(x));
}

static inline Value vm_car(Value pair) {
  if (!is_pair(pair)) abort();
  Pair *p = value_to_heap_object(pair);
  return tagged_value(&p->tag);
}

static inline Value vm_cdr(Value pair) {
  if (!is_pair(pair)) abort();
  Pair *p = value_to_heap_object(pair);
  return p->cdr;
}

static inline void vm_set_car(Value pair, Value val) {
  if (!is_pair(pair)) abort();
  Pair *p = value_to_heap_object(pair);
  gc_write_barrier(gc_ref_from_heap_object(p), sizeof(*p),
                   gc_edge(&p->tag), gc_ref(val.payload));
  tagged_set_value(&p->tag, PAIR_TAG, val);
}

static inline void vm_set_cdr(Value pair, Value val) {
  if (!is_pair(pair)) abort();
  Pair *p = value_to_heap_object(pair);
  gc_write_barrier(gc_ref_from_heap_object(p), sizeof(*p),
                   gc_edge(&p->cdr), gc_ref(val.payload));
  p->cdr = val;
}

static inline Value vm_make_vector(VM vm, Value size, Value *init_loc) {
  if (!is_fixnum(size)) abort();
  intptr_t c_size = fixnum_value(size);
  if (c_size < 0 || c_size > (((uintptr_t)-1) >> 8)) abort();
  size_t bytes = sizeof(Vector) + c_size * sizeof(Value);
  Vector *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes);
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
  if (size > (((uintptr_t)-1) >> 8)) abort();
  size_t bytes = sizeof(Vector) + size * sizeof(Value);
  Vector *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes);
  }
  tagged_set_payload(&ret->tag, VECTOR_TAG, size);
  return value_from_heap_object(ret);
}

static inline int is_vector(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == VECTOR_TAG;
}

static inline Value vm_vector_length(Value vector) {
  if (!is_vector(vector)) abort();
  Vector *v = value_to_heap_object(vector);
  return make_fixnum(tagged_payload(&v->tag));
}

static inline Value vm_vector_ref(Value vector, Value idx) {
  if (!is_vector(vector)) abort();
  if (!is_fixnum(idx)) abort();
  Vector *v = value_to_heap_object(vector);
  size_t size = tagged_payload(&v->tag);
  intptr_t c_idx = fixnum_value(idx);
  if (c_idx < 0 || c_idx >= size) abort();
  return v->vals[c_idx];
}

static inline void vm_vector_set(Value vector, Value idx, Value val) {
  if (!is_vector(vector)) abort();
  if (!is_fixnum(idx)) abort();
  Vector *v = value_to_heap_object(vector);
  size_t size = tagged_payload(&v->tag);
  intptr_t c_idx = fixnum_value(idx);
  if (c_idx < 0 || c_idx >= size) abort();
  gc_write_barrier(gc_ref_from_heap_object(v), sizeof(*v) + sizeof(Value)*size,
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
  if (!is_string(str)) abort();
  String *s = value_to_heap_object(str);
  return value_from_heap_object(s->chars);
}

static inline int is_symbol(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == SYMBOL_TAG;
}

static inline Value vm_symbol_to_string(Value sym) {
  if (!is_symbol(sym)) abort();
  Symbol *s = value_to_heap_object(sym);
  return value_from_heap_object(s->str);
}

static inline Value vm_make_bytevector(VM vm, Value *size, Value *init) {
  if (!is_fixnum(*size)) abort();
  intptr_t c_size = fixnum_value(*size);
  intptr_t c_init = fixnum_value(*init);
  if (c_size < 0 || c_size > (((uintptr_t)-1) >> 8)) abort();
  if (c_init < -128 || c_init > 255) abort();
  size_t bytes = sizeof(Bytevector) + c_size * sizeof(uint8_t);
  Bytevector *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes);
  }
  tagged_set_payload(&ret->tag, BYTEVECTOR_TAG, c_size);
  memset(ret->vals, c_init, c_size);

  return value_from_heap_object(ret);
}

static inline int is_bytevector(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == BYTEVECTOR_TAG;
}

static inline Value vm_bytevector_length(Value bytevector) {
  if (!is_bytevector(bytevector)) abort();
  Bytevector *v = value_to_heap_object(bytevector);
  return make_fixnum(tagged_payload(&v->tag));
}

static inline Value vm_bytevector_u8_ref(Value bytevector, Value idx) {
  if (!is_bytevector(bytevector)) abort();
  if (!is_fixnum(idx)) abort();
  Bytevector *v = value_to_heap_object(bytevector);
  size_t size = tagged_payload(&v->tag);
  intptr_t c_idx = fixnum_value(idx);
  if (c_idx < 0 || c_idx >= size) abort();
  return value_from_fixnum(v->vals[c_idx]);
}

static inline void vm_bytevector_u8_set(Value bytevector, Value idx, Value val) {
  if (!is_bytevector(bytevector)) abort();
  if (!is_fixnum(idx)) abort();
  if (!is_fixnum(val)) abort();
  Bytevector *v = value_to_heap_object(bytevector);
  size_t size = tagged_payload(&v->tag);
  intptr_t c_idx = fixnum_value(idx);
  intptr_t c_val = fixnum_value(val);
  if (c_idx < 0 || c_idx >= size) abort();
  if (c_val < -128 || 255 < c_val) abort();
  v->vals[c_idx] = c_val;
}

static inline Value vm_make_ephemeron(VM vm, Value *key_loc, Value *value_loc) {
  if (!is_heap_object(*key_loc)) abort();
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
  if (!is_ephemeron(ephemeron)) abort();
  Ephemeron *e = value_to_heap_object(ephemeron);
  struct gc_ref k = gc_ephemeron_key(e);
  return gc_ref_is_heap_object(k)
    ? value_from_heap_object(gc_ref_heap_object(k)) : IMMEDIATE_FALSE;
}

static inline Value vm_ephemeron_value(Value ephemeron) {
  if (!is_ephemeron(ephemeron)) abort();
  Ephemeron *e = value_to_heap_object(ephemeron);
  struct gc_ref v = gc_ephemeron_value(e);
  return gc_ref_is_heap_object(v)
    ? value_from_heap_object(gc_ref_heap_object(v)) : IMMEDIATE_FALSE;
}

static inline Value vm_ephemeron_next(Value ephemeron) {
  if (!is_ephemeron(ephemeron)) abort();
  Ephemeron *e = value_to_heap_object(ephemeron);
  Ephemeron *next = gc_ephemeron_chain_next(e);
  return next ? value_from_heap_object(next) : IMMEDIATE_FALSE;
}

static inline void vm_ephemeron_kill(Value ephemeron) {
  if (!is_ephemeron(ephemeron)) abort();
  Ephemeron *e = value_to_heap_object(ephemeron);
  gc_ephemeron_mark_dead(e);
}

static inline Value vm_make_ephemeron_table(VM vm, Value *size_loc) {
  if (!is_fixnum(*size_loc)) abort();
  intptr_t c_size = fixnum_value(*size_loc);
  if (c_size < 0 || c_size > (((uintptr_t)-1) >> 8)) abort();
  size_t bytes = sizeof(EphemeronTable) + c_size * sizeof(Value);
  EphemeronTable *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm_record_cooperative_safepoint(vm);
    ret = gc_allocate_slow(vm.thread->mut, bytes);
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
  if (!is_ephemeron_table(table)) abort();
  EphemeronTable *t = value_to_heap_object(table);
  return value_from_fixnum(tagged_payload(&t->tag));
}

static inline Value vm_ephemeron_table_ref(Value table, Value idx) {
  if (!is_ephemeron_table(table)) abort();
  EphemeronTable *t = value_to_heap_object(table);
  if (!is_fixnum(idx)) abort();
  size_t size = tagged_payload(&t->tag);
  intptr_t c_idx = fixnum_value(idx);
  if (c_idx < 0 || c_idx >= size) abort();
  Ephemeron *e = gc_ephemeron_chain_head(&t->vals[c_idx]);
  return e ? value_from_heap_object(e) : IMMEDIATE_FALSE;
}

static inline void vm_ephemeron_table_push(Value table, Value idx, Value ephemeron) {
  if (!is_ephemeron_table(table)) abort();
  if (!is_ephemeron(ephemeron)) abort();
  EphemeronTable *t = value_to_heap_object(table);
  Ephemeron *e = value_to_heap_object(ephemeron);
  if (!is_fixnum(idx)) abort();
  size_t size = tagged_payload(&t->tag);
  intptr_t c_idx = fixnum_value(idx);
  if (c_idx < 0 || c_idx >= size) abort();
  gc_write_barrier(gc_ref_from_heap_object(t), sizeof(*t) + sizeof(Value)*size,
                   gc_edge(&t->vals[c_idx]), gc_ref_from_heap_object(e));
  gc_ephemeron_chain_push(&t->vals[c_idx], e);
}

static inline Value vm_char_to_integer(Value x) {
  return value_from_fixnum(value_to_char(x));
}

static inline Value vm_integer_to_char(Value x) {
  intptr_t i = value_to_fixnum(x);
  if (i < 0 || i >= (1 << 21)) __builtin_trap();
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
  if (!is_fixnum(a)) abort();
  if (!is_fixnum(b)) abort();
  return fixnum_value(a) < fixnum_value(b);
}

static inline int vm_is_numerically_eq(Value a, Value b) {
  if (!is_fixnum(a)) abort();
  if (!is_fixnum(b)) abort();
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
    abort();
  }
}

static inline int is_thread(Value x) {
  return is_heap_object(x) && tagged_kind(value_to_heap_object(x)) == THREAD_TAG;
}

static void* vm_thread_transition_to_running(void *data) {
  Thread *thread = data;
  pthread_mutex_lock(&thread->lock);
  thread->state = THREAD_WAITING;
  pthread_cond_signal(&thread->cond);
  do
    pthread_cond_wait(&thread->cond, &thread->lock);
  while (thread->state == THREAD_WAITING);
  if (thread->state != THREAD_RUNNING) abort();
  pthread_mutex_unlock(&thread->lock);
  return NULL;
}

static void* vm_thread_wait_for_stopping(void *data) {
  Thread *thread = data;
  pthread_mutex_lock(&thread->lock);
  while (thread->state < THREAD_STOPPING)
    pthread_cond_wait(&thread->cond, &thread->lock);
  if (thread->state != THREAD_STOPPING) abort();
  // Keep lock.
  return NULL;
}

static void* vm_thread_transition_to_stopped(void *data) {
  Thread *thread = data;
  pthread_mutex_lock(&thread->lock);
  thread->state = THREAD_STOPPING;
  pthread_cond_signal(&thread->cond);
  while (thread->state == THREAD_STOPPING)
    pthread_cond_wait(&thread->cond, &thread->lock);
  if (thread->state != THREAD_STOPPED) abort();
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
  if (vm.sp != thread->sp_base - 1) abort();
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
  struct gc_heap *heap = data;

  Thread *thread = calloc(1, sizeof(Thread));
  pthread_mutex_init(&thread->lock, NULL);
  pthread_cond_init(&thread->cond, NULL);
  thread->state = THREAD_SPAWNING;

  struct vm_spawn_thread_data spawn_data = { heap, thread };
  pthread_mutex_lock(&thread->lock);
  pthread_create(&thread->tid, NULL, vm_thread_proc, &spawn_data);
  while (thread->state == THREAD_SPAWNING)
    pthread_cond_wait(&thread->cond, &thread->lock);

  if (thread->state != THREAD_WAITING) abort();

  return thread;
}

static inline Value vm_spawn_thread(struct VM vm, Value *thunk) {
  if (!is_closure(*thunk)) abort();

  vm_record_cooperative_safepoint(vm);

  Thread *thread = gc_call_without_gc(vm.thread->mut,
                                      vm_spawn_thread_without_gc,
                                      vm.thread->heap);

  thread->sp_base[-1] = *thunk;
  thread->state = THREAD_RUNNING;
  pthread_cond_signal(&thread->cond);
  pthread_mutex_unlock(&thread->lock);

  ThreadHandle *handle = gc_allocate(vm.thread->mut, sizeof(ThreadHandle));
  tagged_set_payload(&handle->tag, THREAD_TAG, 0);
  handle->thread = thread;
  return value_from_heap_object(handle);
}

static inline Value vm_join_thread(struct VM vm, Value *handle) {
  if (!is_thread(*handle)) abort();
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
  if (gettimeofday(&t, NULL) == -1) abort();
  unsigned long usec = t.tv_sec * 1000 * 1000 + t.tv_usec;
  if (usec > (unsigned long)FIXNUM_MAX) abort();
  return value_from_fixnum(usec);
}

static inline void vm_die(void) {
  fflush(stdout);
  abort();
}

static inline void vm_wrong_num_args(size_t expected, size_t actual) {
  fflush(stdout);
  fprintf(stderr, "wrong number of args to procedure (expected %zu, got %zu)\n",
          expected, actual);
  exit(1);
}

static inline Value vm_gc_collect(struct VM vm) {
  gc_collect(vm.thread->mut);
  return IMMEDIATE_TRUE;
}

#endif // WHIFFLE_VM_H
