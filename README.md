# Whiffle Scheme

Oh my god another Scheme.  Don't worry, this one is useless!

## What it is

A simple Scheme compiler, made as a workbench for experimenting with
garbage collectors.  Whiffle Scheme re-uses the front-end of
[Guile](https://gnu.org/s/guile/) and compiles to C.

Whiffle is a [baseline
compiler](https://wingolog.org/archives/2020/06/03/a-baseline-compiler-for-guile);
expressive optimizations are out of reach.  It does use Guile's
front-end optimizers, though, notably
[peval](https://wingolog.org/archives/2011/10/11/partial-evaluation-in-guile),
basically as a syntactic convenience; we want to be able to write
low-level garbage collection benchmarks, but it's nice to take advantage
of a reliable inliner so that these benchmarks are not such a pain to
write.

Currently Whiffle uses the C stack for control (function calls) and a
side stack for values (locals and temporaries).

To the garbage collector, a thread's VM state is basically just a stack
pointer that grows down.  There are no other ABI registers.  Each
function has a compile-time-determined maximum number of temporaries and
locals, and will ensure there is enough space on function entry, bumping
the stack pointer accordingly.  Operations load operands from and store
results to stack slots, without moving the stack pointer.  Only
single-valued function returns are supported.

If an operation can allocate, and allocation would take the slow path to
call into the collector, the stack is trimmed so that the stack is
packed, with no trailing garbage words.  In this way we don't have to do
any book-keeping as to what stack slot is live.  Using a side stack does
inhibit some C compiler optimizations though, relative to the alternate
strategy of using C local variables; the compiler doesn't know that
bumping the stack pointer is an `alloca`.

Using the C stack for control means no delimited continuations, at least
for now.  A proper solution that would allow for delimited continuations
would be something like the `tailify` pass used by
[Hoot](https://gitlab.com/spritely/guile-hoot/).  Maybe someday we'd do
that, if we actually care about completeness.  But for now, Whiffle is
just a workbench for making GC benchmarks.

## Using Whiffle

Right now the paradigm is, you have to compile a function; the root
expression of your compilation must be a `lambda`.  The compiled program
will take its arguments from the command line.  This way you can be sure
that any input to the benchmark (because Whiffle is used to make
benchmarks) is not part of the compilation unit of the benchmark itself.

So one example is to compile a function to C:

```
$ whiffle -c -e '(lambda () 42)'
```

Actually when you are running in the source tree, probably you want to
wrap in `pre-inst-env`:

```
$ ./pre-inst-env whiffle -c -e '(lambda () 42)'
#include "whiffle/vm.h"

static VM F0 (VM vm, size_t nargs);
static Closure C0;

static VM F0 (VM vm, size_t nargs) {
  if (nargs != 1) abort();
  if (vm.sp - vm.thread->sp_limit < 1) abort();
  vm.sp -= 1;
  vm.sp[1] = IMMEDIATE_INTEGER (42);
  return vm_trim(vm, 1);
}

int main (int argc, char *argv[]) {
  if (argc != 1) abort();
  C0 = STATIC_CLOSURE(F0);
  Thread thread;
  VM vm = vm_prepare_thread(&thread, 1);
  vm.sp[0] = STATIC_REF (C0);
  vm = F0(vm, 1);
  vm_print_value(vm.sp[0]);
  return 0;
};
```

The generated C uses operations from
[`whiffle/vm.h`](./include/whiffle/vm.h).

Compiling... I have gotten it to compile but I need to leave this here
for tonight; will come back to it later.  See, Whiffle is mainly made
for [Whippet](https://github.com/wingo/whippet-gc/).  Whippet is an
abstract GC API, and also a number of implementations that can be
selected at compile-time, and also one blessed implementation which is
itself called Whippet.  A little confusing, but there you go.  Whippet
is designed to be an embed-only library.  I'm still working on the
Whippet compilation ergonomics; right now to compile Whippet, besides
some generic files you need to compile a the GC implementation that you
select, but specialized to the program at hand: how it represents tagged
values, how to install a forwarding pointer, and so on.  This will work!
But it is a mess for the moment.  Come back in a month and see where we
are :)
