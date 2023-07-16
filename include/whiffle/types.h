#ifndef WHIFFLE_TYPES_H
#define WHIFFLE_TYPES_H

#include <stddef.h>
#include <stdint.h>

struct Value { uintptr_t payload; };
typedef struct Value Value;

typedef struct Thread Thread;
typedef struct VM {
  Thread *thread;
  Value *sp;
} VM;

typedef VM (*Code)(VM, size_t);

typedef union Tag {uintptr_t tag; Value inline_value;} Tag;
typedef struct Pair { Tag tag; Value cdr; } Pair;
typedef struct Box { Tag tag; Value val; } Box;
typedef struct Vector { Tag tag; Value vals[]; } Vector;
typedef struct Closure { Tag tag; Code code; Value free_vars[]; } Closure;

#define FIXNUM_MAX ((intptr_t)(((uintptr_t)-1)>>2))
#define FIXNUM_MIN (-FIXNUM_MAX-1)

#define FIXNUM_TAG ((uintptr_t)2)
#define PAIR_TAG ((uintptr_t)0)
#define BOX_TAG ((uintptr_t)0x07)
#define VECTOR_TAG ((uintptr_t)0x0d)
#define CLOSURE_TAG ((uintptr_t)0x45)

#define STATIC_REF(label) ((Value){(uintptr_t)&label})
#define STATIC_PAIR(a, b) ((Pair){(Tag){(Value){a.payload | PAIR_TAG}}, b})
#define STATIC_VECTOR(len, ...) ((Vector){(Tag){VECTOR_TAG|(len<<8)}, {__VA_ARGS__}})
#define STATIC_CLOSURE(label) ((Closure){(Tag){CLOSURE_TAG}, &label})

#define IMMEDIATE_INTEGER(i)  ((Value){(uintptr_t)((i << 2) | FIXNUM_TAG)})
#define IMMEDIATE_FALSE       ((Value){(uintptr_t)0x004})
#define IMMEDIATE_TRUE        ((Value){(uintptr_t)0x404})
#define IMMEDIATE_UNSPECIFIED ((Value){(uintptr_t)0x804})

struct gc_mutator_roots {
  VM safepoint;
};

typedef struct Thread {
  Value *sp_base;
  Value *sp_limit;
  struct gc_mutator *mut;
  struct gc_mutator_roots roots;
} Thread;

static inline int is_heap_object(Value v) {
  return (v.payload & 3) == 0;
}

static inline Value value_from_heap_object(void *obj) {
  return (Value){(uintptr_t)obj};
}

static inline void* value_to_heap_object(Value v) {
  if (!is_heap_object(v)) abort();
  return (void*)v.payload;
}

static inline int is_fixnum(Value v) {
  return (v.payload & 3) == FIXNUM_TAG;
}

static inline intptr_t value_to_fixnum(Value v) {
  if (!is_fixnum(v)) abort();
  return ((intptr_t)v.payload) >> 2;
}

static inline Value value_from_fixnum(intptr_t v) {
  return (Value){(v << 2) | 2};
}

static inline uint8_t tag_kind(Tag* tag) {
  return tag->tag & 0xff;
}

static inline Value tag_value(Tag *tag) {
  return tag->inline_value;
}

static inline uintptr_t tag_payload(Tag *tag) {
  return tag->tag >> 8;
}

static inline void tag_set_payload(Tag *tag, uint8_t kind, uintptr_t payload) {
  tag->tag = ((uintptr_t)kind) | (payload << 8);
}

static inline void tag_set_value(Tag *tag, uint8_t kind, Value v) {
  tag->tag = v.payload | kind;
}

#endif // WHIFFLE_TYPES_H
