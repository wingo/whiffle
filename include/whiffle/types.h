#ifndef WHIFFLE_TYPES_H
#define WHIFFLE_TYPES_H

#include <stddef.h>
#include <stdint.h>
#include <pthread.h>

struct Value { uintptr_t payload; };
typedef struct Value Value;

typedef struct Thread Thread;
typedef struct VM {
  Thread *thread;
  Value *sp;
} VM;

typedef VM (*Code)(VM, size_t);

struct gc_ephemeron;

typedef union Tagged {uintptr_t tag; Value inline_value;} Tagged;
typedef struct Pair { Tagged tag; Value cdr; } Pair;
typedef struct Box { Tagged tag; Value val; } Box;
typedef struct Vector { Tagged tag; Value vals[]; } Vector;
typedef struct String { Tagged tag; Vector *chars; } String;
typedef struct Symbol { Tagged tag; String *str; } Symbol;
typedef struct Closure { Tagged tag; Code code; Value free_vars[]; } Closure;
// FIXME: a finalizer on ThreadHandle would allow cleanup of the Thread.
typedef struct ThreadHandle { Tagged tag; Thread *thread; } ThreadHandle;
typedef struct Bytevector { Tagged tag; uint8_t vals[]; } Bytevector;
typedef struct gc_ephemeron Ephemeron;
typedef struct EphemeronTable { Tagged tag; Ephemeron *vals[]; } EphemeronTable;

#define FIXNUM_MAX ((intptr_t)(((uintptr_t)-1)>>2))
#define FIXNUM_MIN (-FIXNUM_MAX-1)

/* These tags are chosen to mostly match Guile's tagging scheme.  It
   would be better if the fixnum tag were zero, but that's not how Guile
   does it.  */
#define FIXNUM_VALUE_TAG ((uintptr_t)2)    /* 0b0010 */
#define CHAR_VALUE_TAG   ((uintptr_t)0xc)  /* 0b1100 */

#define PAIR_MASK        ((uintptr_t)0x01) /* 0b0000_0001 */
#define PAIR_TAG         ((uintptr_t)0)    /* 0b0000_0000 */

#define FORWARDED_MASK   ((uintptr_t)0x07) /* 0b0000_0111 */
#define FORWARDED_TAG    ((uintptr_t)0x03) /* 0b0000_0011 */

#define BOX_TAG          ((uintptr_t)0x05) /* 0b0000_0101 */
#define VECTOR_TAG       ((uintptr_t)0x07) /* 0b0000_0111 */
#define CLOSURE_TAG      ((uintptr_t)0x0d) /* 0b0000_1101 */
#define STRING_TAG       ((uintptr_t)0x15) /* 0b0001_0101 */
#define SYMBOL_TAG       ((uintptr_t)0x17) /* 0b0001_0111 */
#define THREAD_TAG       ((uintptr_t)0x1d) /* 0b0001_1101 */
#define BYTEVECTOR_TAG   ((uintptr_t)0x25) /* 0b0010_0101 */
#define EPHEMERON_TAG    ((uintptr_t)0x27) /* 0b0010_0111 */
#define EPHEMERON_TABLE_TAG ((uintptr_t)0x2d) /* 0b0010_1101 */
#define BUSY_TAG         ((uintptr_t)-1)   /* all 1's */

#define REMEMBERED_FLAG  ((uintptr_t)0x80) /* 0b1000_0000 */

#define STATIC_CODE(label) ((uintptr_t)&label)

#define STATIC_PAIR(a, b)       {{a | PAIR_TAG}, {b}}
#define STATIC_VECTOR(len, ...) {{VECTOR_TAG|(len<<8)}, {__VA_ARGS__}}
#define STATIC_STRING(v)        {{STRING_TAG}, (Vector*)v}
#define STATIC_SYMBOL(s)        {{SYMBOL_TAG}, (String*)s}
#define STATIC_CLOSURE(label)   {{CLOSURE_TAG}, &label}
#define STATIC_BYTEVECTOR(len, ...) {{BYTEVECTOR_TAG|(len<<8)}, {__VA_ARGS__}}

#define IMMEDIATE_INTEGER_CODE(i)  ((uintptr_t)((i << 2) | FIXNUM_VALUE_TAG))
#define IMMEDIATE_CHAR_CODE(i)     ((uintptr_t)((((uintptr_t)i) << 4) | CHAR_VALUE_TAG))
#define IMMEDIATE_FALSE_CODE       ((uintptr_t)0x004)
#define IMMEDIATE_TRUE_CODE        ((uintptr_t)0x404)
#define IMMEDIATE_UNSPECIFIED_CODE ((uintptr_t)0x804)
#define IMMEDIATE_NULL_CODE        ((uintptr_t)0xc04)

#define CONST(code) ((Value){code})

#define IMMEDIATE_INTEGER(i)  CONST(IMMEDIATE_INTEGER_CODE(i))
#define IMMEDIATE_CHAR(i)     CONST(IMMEDIATE_CHAR_CODE(i))
#define IMMEDIATE_FALSE       CONST(IMMEDIATE_FALSE_CODE)
#define IMMEDIATE_TRUE        CONST(IMMEDIATE_TRUE_CODE)
#define IMMEDIATE_UNSPECIFIED CONST(IMMEDIATE_UNSPECIFIED_CODE)
#define IMMEDIATE_NULL        CONST(IMMEDIATE_NULL_CODE)

struct gc_mutator_roots {
  VM safepoint;
};

struct gc_address_set;
struct gc_extern_space {
  pthread_mutex_t lock;
  struct gc_address_set *marked;
};

enum thread_state {
  THREAD_SPAWNING,
  THREAD_WAITING,
  THREAD_RUNNING,
  THREAD_STOPPING,
  THREAD_STOPPED,
  THREAD_ERROR
};

struct prng_state {
  // xoshiro256**
  uint64_t s[4];
};

typedef struct Thread {
  Value *sp_base;
  Value *sp_limit;
  struct gc_mutator *mut;
  struct gc_heap *heap;
  struct gc_mutator_roots roots;
  struct gc_extern_space extern_space;
  pthread_t tid;
  pthread_mutex_t lock;
  pthread_cond_t cond;
  enum thread_state state;
  struct prng_state prng_state;
} Thread;

static inline int is_heap_object(Value v) {
  return (v.payload & 7) == 0;
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

static inline int is_char(Value v) {
  return (v.payload & 0xf) == CHAR_VALUE_TAG;
}

static inline intptr_t value_to_fixnum(Value v) {
  if (!is_fixnum(v)) __builtin_trap();
  return ((intptr_t)v.payload) >> 2;
}

static inline uint32_t value_to_char(Value v) {
  if (!is_char(v)) __builtin_trap();
  return ((uint32_t)v.payload) >> 4;
}

static inline Value value_from_fixnum(intptr_t v) {
  return IMMEDIATE_INTEGER(v);
}

static inline Value value_from_char(uint32_t v) {
  return IMMEDIATE_CHAR(v);
}

static inline uint8_t tag_kind(uintptr_t tag) {
  return tag & 0xff;
}

static inline uint8_t tagged_kind(Tagged* tagged) {
  return tag_kind(tagged->tag);
}

static inline uint8_t tag_is_pair(uintptr_t tag) {
  return (tag & PAIR_MASK) == PAIR_TAG;
}

static inline uint8_t tagged_is_pair(Tagged* tagged) {
  return tag_is_pair(tagged->tag);
}

static inline int tagged_remembered(Tagged* tagged) {
  return tagged->tag & REMEMBERED_FLAG;
}

static inline void tagged_set_remembered(Tagged* tagged) {
  tagged->tag |= REMEMBERED_FLAG;
}

static inline void tagged_clear_remembered(Tagged* tagged) {
  tagged->tag &= ~(uintptr_t)REMEMBERED_FLAG;
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

static inline uintptr_t tag_payload(uintptr_t tag) {
  return tag >> 8;
}

static inline uintptr_t tagged_payload(Tagged* tagged) {
  return tag_payload(tagged->tag);
}

static inline void tagged_set_payload(Tagged* tagged, uint8_t kind, uintptr_t payload) {
  tagged->tag = ((uintptr_t)kind) | (payload << 8);
}

static inline void tagged_set_value(Tagged* tagged, uint8_t kind, Value v) {
  tagged->tag = v.payload | kind;
}

#endif // WHIFFLE_TYPES_H
