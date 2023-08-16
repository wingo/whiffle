# Whiffle design document

## Problem statement

The [Whippet garbage collector
library](https://github.com/wingo/whippet) needs some microbenchmarks.
This turns out to be surprisingly tricky.

Some GC microbenchmarks can written in C, but C is an annoying language
for writing garbage-collected programs if you want precise root-finding
to be an option.  Really what a GC library needs is microbenchmarks
written in a language with automatically-managed memory.  But, such
languages tend to have large run-times with variable performance
characteristics; it would be nice to have something smaller.  And, it
needs to link well with C.

Whiffle is that something.  It compiles Scheme programs to native
binaries, outputting C as an intermediate stage.  The C is
straightforward and self-contained, just including a few files from
Whiffle's source.

## Structure

Whiffle only supports a subset of Scheme: enough to write the
microbenchmarks.

When writing Scheme, you want a baseline level of abstraction: macros,
inlining of trivial functions, basic constant folding and unrolling, and
so on: the [macro writer's bill of
rights](https://www.youtube.com/watch?v=LIEX3tUliHw), basically.
Whiffle has this by re-using the front-end of
[Guile](https://gnu.org/s/guile/).  That's easy to do, given that the
Whiffle compiler itself is written in Guile's dialect of Scheme.

As far as the back-end goes, Whiffle is a [baseline
compiler](https://wingolog.org/archives/2020/06/03/a-baseline-compiler-for-guile).
It compiles by walking the abstract syntax tree of a program, emitting
code as it goes.

The C code that Whiffle emits currently uses the C stack for control
(function calls) and a side stack for values (locals and temporaries).

## Stack roots

To the garbage collector, a Whiffle thread's VM state is basically just
a stack pointer that grows down.  There are no other ABI registers.
Each function has a compile-time-determined maximum number of
temporaries and locals, and will ensure there is enough space on
function entry, bumping the stack pointer accordingly.  Operations load
operands from and store results to stack slots, without moving the stack
pointer.  Only single-valued function returns are supported.

If an operation can allocate, and allocation would take the slow path to
call into the collector, the stack is trimmed so that the stack is
packed, with no trailing garbage words.  In this way we don't have to do
any book-keeping as to what stack slot is live.  Using a side stack does
inhibit some C compiler optimizations though, relative to the alternate
strategy of using C local variables; the compiler doesn't know that
bumping the stack pointer is an `alloca`.

## Limitations

Using the C stack for control means no delimited continuations, at least
for now.  A proper solution that would allow for delimited continuations
would be something like the `tailify` pass used by
[Hoot](https://gitlab.com/spritely/guile-hoot/).  Maybe someday we'd do
that, if we actually care about completeness.  But for now, Whiffle is
just a workbench for making GC benchmarks.

A garbage collector library needs macrobenchmarks in addition to
microbenchmarks; these will come to Whippet when it is integrated into
[Guile Scheme](https://gnu.org/s/guile).  But until then, Whiffle is
here.

Right now the set of primitives supported by Whiffle is verrrrrrry
limited.

The error-handling is terrible: Whiffle generally just aborts.  But, it
should be safe: it shouldn't be possible to access, say, a pair as if it
were a vector.

