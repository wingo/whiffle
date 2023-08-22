#ifndef WHIFFLE_VM_H
#define WHIFFLE_VM_H

#include <errno.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/mman.h>

#include <gc-api.h>

#include "types.h"

static inline intptr_t fixnum_value(Value v) { return value_to_fixnum(v); }
static inline Value make_fixnum(intptr_t v) { return value_from_fixnum(v); }

static VM vm_prepare_main_thread(Thread *thread, size_t initial_nargs) {
  size_t bytes = 2 * 1024 * 1024;
  if (initial_nargs > bytes / sizeof(Value)) abort();
  void *mem = mmap(NULL, bytes, PROT_READ|PROT_WRITE,
                   MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (mem == MAP_FAILED) {
    perror("allocating thread stack failed");
    GC_CRASH();
  }
  thread->sp_base = (Value*)(((char *)mem) + bytes);
  thread->sp_limit = mem;
  struct gc_options *options = gc_allocate_options();
  char *options_str = getenv("GC_OPTIONS");
  if (options_str) {
    if (!gc_options_parse_and_set_many(options, options_str)) {
      fprintf(stderr, "failed to set options: %s\n", options_str);
      GC_CRASH();
    }
  }
  if (!gc_init(options, NULL, &thread->heap, &thread->mut))
    GC_CRASH();

  gc_mutator_set_roots(thread->mut, &thread->roots);
  Value *sp = thread->sp_base - initial_nargs;
  VM vm = (VM){thread, sp};
  thread->roots.safepoint = vm;

  memset(&thread->extern_space, 0, sizeof(thread->extern_space));
  gc_heap_set_extern_space(thread->heap, &thread->extern_space);

  return vm;
}

static inline VM vm_trim(VM vm, size_t n) {
  return ((VM){vm.thread, vm.sp + n});
}

static inline Value vm_make_closure(VM vm, Code code, size_t nfree) {
  size_t bytes = sizeof(Closure) + nfree * sizeof(Value);
  Closure *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm.thread->roots.safepoint = vm;
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
    vm.thread->roots.safepoint = vm;
    ret = gc_allocate_slow(vm.thread->mut, bytes);
  }
  tagged_set_payload(&ret->tag, BOX_TAG, 0);
  ret->val = *val_loc;
  return value_from_heap_object(ret);
}

static inline void vm_box_set(Value box, Value val) {
  Box *b = value_to_heap_object(box);
  b->val = val;
}

static inline Value vm_box_ref(Value box) {
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
    vm.thread->roots.safepoint = vm;
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
  tagged_set_value(&p->tag, PAIR_TAG, val);
}

static inline void vm_set_cdr(Value pair, Value val) {
  if (!is_pair(pair)) abort();
  Pair *p = value_to_heap_object(pair);
  p->cdr = val;
}

static inline Value vm_make_vector(VM vm, Value size, Value *init_loc) {
  if (!is_fixnum(size)) abort();
  intptr_t c_size = fixnum_value(size);
  if (c_size < 0 || c_size > (((uintptr_t)-1) >> 8)) abort();
  size_t bytes = sizeof(Vector) + c_size * sizeof(Value);
  Vector *ret = gc_allocate_fast(vm.thread->mut, bytes);
  if (GC_UNLIKELY(!ret)) {
    vm.thread->roots.safepoint = vm;
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
    vm.thread->roots.safepoint = vm;
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
  v->vals[c_idx] = val;
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

static inline Value vm_char_to_integer(Value x) {
  return value_from_fixnum(value_to_char(x));
}

static inline Value vm_integer_to_char(Value x) {
  intptr_t i = value_to_fixnum(x);
  if (i < 0 || i >= (1 << 21)) __builtin_trap();
  return value_from_fixnum(value_to_char(x));
}

static inline void vm_write_char(Value ch) {
  fputc(value_to_char(ch), stdout);
}

static inline int vm_is_false(Value val) {
  return val.payload == IMMEDIATE_FALSE.payload;
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

#endif // WHIFFLE_VM_H
