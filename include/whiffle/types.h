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

typedef union Tagged {uintptr_t tag; Value inline_value;} Tagged;
typedef struct Pair { Tagged tag; Value cdr; } Pair;
typedef struct Box { Tagged tag; Value val; } Box;
typedef struct Vector { Tagged tag; Value vals[]; } Vector;
typedef struct Closure { Tagged tag; Code code; Value free_vars[]; } Closure;

#define FIXNUM_MAX ((intptr_t)(((uintptr_t)-1)>>2))
#define FIXNUM_MIN (-FIXNUM_MAX-1)

/* These tags are chosen to mostly match Guile's tagging scheme.  It
   would be better if the fixnum tag were zero, but that's not how Guile
   does it.  */
#define FIXNUM_VALUE_TAG ((uintptr_t)2)    /* 0b0010 */

#define PAIR_MASK        ((uintptr_t)0x01) /* 0b0001 */
#define PAIR_TAG         ((uintptr_t)0)    /* 0b0000 */

#define FORWARDED_MASK   ((uintptr_t)0x07) /* 0b0111 */
#define FORWARDED_TAG    ((uintptr_t)0x03) /* 0b0011 */

#define BOX_TAG          ((uintptr_t)0x05) /* 0b0101 */
#define VECTOR_TAG       ((uintptr_t)0x07) /* 0b0111 */
#define CLOSURE_TAG      ((uintptr_t)0x0d) /* 0b1101 */
#define BUSY_TAG         ((uintptr_t)0x0f) /* 0b1111 */

#define REMEMBERED_TAG   ((uintptr_t)0x10)

#define STATIC_REF(label) ((Value){(uintptr_t)&label})
#define STATIC_PAIR(a, b) ((Pair){(Tagged){(Value){a.payload | PAIR_TAG}}, b})
#define STATIC_VECTOR(len, ...) ((Vector){(Tagged){VECTOR_TAG|(len<<8)}, {__VA_ARGS__}})
#define STATIC_CLOSURE(label) ((Closure){(Tagged){CLOSURE_TAG}, &label})

#define IMMEDIATE_INTEGER(i)  ((Value){(uintptr_t)((i << 2) | FIXNUM_VALUE_TAG)})
#define IMMEDIATE_FALSE       ((Value){(uintptr_t)0x004})
#define IMMEDIATE_TRUE        ((Value){(uintptr_t)0x404})
#define IMMEDIATE_UNSPECIFIED ((Value){(uintptr_t)0x804})
#define IMMEDIATE_NULL        ((Value){(uintptr_t)0xc04})

struct gc_mutator_roots {
  VM safepoint;
};

typedef struct Thread {
  Value *sp_base;
  Value *sp_limit;
  struct gc_mutator *mut;
  struct gc_heap *heap;
  struct gc_mutator_roots roots;
} Thread;

static inline int is_heap_object(Value v) {
  return (v.payload & 3) == 0;
}

static inline Value value_from_heap_object(void *obj) {
  return (Value){(uintptr_t)obj};
}

static inline void* value_to_heap_object(Value v) {
  if (!is_heap_object(v)) __builtin_trap();
  return (void*)v.payload;
}

static inline int is_fixnum(Value v) {
  return (v.payload & 3) == FIXNUM_VALUE_TAG;
}

static inline intptr_t value_to_fixnum(Value v) {
  if (!is_fixnum(v)) __builtin_trap();
  return ((intptr_t)v.payload) >> 2;
}

static inline Value value_from_fixnum(intptr_t v) {
  return (Value){(v << 2) | 2};
}

static inline uint8_t tagged_kind(Tagged* tagged) {
  return tagged->tag & 0x0f;
}

static inline uint8_t tagged_is_pair(Tagged* tagged) {
  return (tagged->tag & PAIR_MASK) == PAIR_TAG;
}

static inline int tagged_remembered(Tagged* tagged) {
  return tagged->tag & REMEMBERED_TAG;
}

static inline void tagged_set_remembered(Tagged* tagged) {
  tagged->tag |= REMEMBERED_TAG;
}

static inline void tagged_clear_remembered(Tagged* tagged) {
  tagged->tag &= ~(uintptr_t)REMEMBERED_TAG;
}

static inline int tag_is_forwarded(uintptr_t tag) {
  return (tag & FORWARDED_MASK) == FORWARDED_TAG;
}

static inline uintptr_t make_forwarded_tag(Tagged* new_loc) {
  return FORWARDED_TAG | ((uintptr_t) new_loc);
}

static inline uintptr_t tag_forwarded_addr(uintptr_t tag) {
  return tag - FORWARDED_TAG;
}

static inline Value tagged_value(Tagged* tagged) {
  return tagged->inline_value;
}

static inline uintptr_t tagged_payload(Tagged* tagged) {
  return tagged->tag >> 8;
}

static inline void tagged_set_payload(Tagged* tagged, uint8_t kind, uintptr_t payload) {
  tagged->tag = ((uintptr_t)kind) | (payload << 8);
}

static inline void tagged_set_value(Tagged* tagged, uint8_t kind, Value v) {
  tagged->tag = v.payload | kind;
}

#endif // WHIFFLE_TYPES_H
