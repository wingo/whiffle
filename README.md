# Whiffle Scheme

Oh my god another Scheme.  Don't worry, this one is useless!

## What it is

A simple Scheme compiler, made as a workbench for experimenting with
garbage collectors.  See the [design document](./doc/design.md), for more.

## Using Whiffle

Before starting, maybe you want to compile the Whiffle compiler:

```
$ make
[...]
```

You don't have to do this, but things run faster this way.

Whiffle itself has three dependencies: [Guile
3.0](https://gnu.org/s/guile), make, and a C compiler.  It embeds a copy
of [Whippet](https://github.com/wingo/whippet), a portable garbage
collector implementation.  (Testing Whippet is the purpose of Whiffle.)
The Whippet GC currently only works on Linux systems, but this will be
fixed to include MacOS and Windows soonish.  Other OS implementations
are welcome but they should go through the Whippet project first.

Right now the Whiffle paradigm is, you have to compile a function: the
root expression of your compilation must be a `lambda`.  The compiled
program will take its arguments from the command line.  This way you can
be sure that any input to the benchmark (because Whiffle is used to make
benchmarks) is not part of the compilation unit of the benchmark itself.

So one example is to compile a function to C, then run the compiled
file:

```
$ ./pre-inst-env whiffle -e '42'
```

The `pre-inst-env` is for running Whiffle from within the source tree,
which is the only supported way to run it, currently.

But if you ran that, OK, actually we should print the result:

```
$ ./pre-inst-env whiffle -e '(writeln 42)'
```

You may prefer instead to generate a binary instead of running it
directly:

```
$ ./pre-inst-env whiffle -o foo -e '(write 42)'
$ ./foo
42
```

The resulting binary has some standard command-line options:

```
$ ./foo --help
usage: ./foo [--print-stats] [--gc-options OPTIONS] ARG...
```

For example, we can print some statistics:

```
$ ./foo --print-stats
42
Completed 0 major collections (0 minor).
0.083 ms total time (0.000 stopped).
Heap size is 6.291 MB (max 6.291 MB); peak live data 0.000 MB.
```

Whiffle's GC library, [Whippet](https://github.com/wingo/whippet),
includes a number of concrete garbage collectors.  The choice of which
collector to use is made at build-time:

```
$ ./pre-inst-env whiffle -o foo -e '(write 42)' --gc=help
available GC implementations:
  semi                   serial copying
  bdw                    third-party BDW-GC parallel mark-sweep
  whippet                serial immix
  generational-whippet   whippet + in-place generations
  parallel-whippet       whippet + parallel tracing
  stack-conservative-whippet
                         whippet + conservative stack root finding
  heap-conservative-whippet
                         stack-conservative-whippet + conservative heap edges
  stack-conservative-parallel-whippet
  heap-conservative-parallel-whippet
  stack-conservative-generational-whippet
  heap-conservative-generational-whippet
  parallel-generational-whippet
  stack-conservative-parallel-generational-whippet
  heap-conservative-parallel-generational-whippet
                         combinations of the above
```

The default collector is `semi`.  The collectors are also parameterized
by run-time
[options](https://github.com/wingo/whippet/blob/main/doc/manual.md#options).
To set options, pass the `--gc-options` argument:

```
$ ./foo --print-stats --gc-options heap-size=100m
warning: parallelism unimplemented in semispace copying collector
42
Completed 0 major collections (0 minor).
0.050 ms total time (0.000 stopped).
Heap size is 104.858 MB (max 104.858 MB); peak live data 0.000 MB.
```
For example

One of the options is `parallelism`, which defaults to the number of
cores available.  But `semi` only supports 1 core, currently; it will
warn if the `parallelism` option is not 1.  (We have been omitting the
warning from the output above, except in this latest example.)  So if
you want to avoid the warning you can pass `parallelism=1` as an option:

```
$ ./foo --print-stats --gc-options heap-size=100m,parallelism=1
42
Completed 0 major collections (0 minor).
0.065 ms total time (0.000 stopped).
Heap size is 104.858 MB (max 104.858 MB); peak live data 0.000 MB.
```

Whiffle can compile files instead of expressions:

```
$ ./pre-inst-env whiffle examples/peano-fib.scm 25
warning: parallelism unimplemented in semispace copying collector
121393
```

Here we see that we got the semi parallelism warning.  When we use
whiffle to evaluate a file or expression directly instead of reifying a
binary, we can pass some command-line arguments to set relevant GC
options: `--parallelism` for a limit to the number of GC threads,
`--heap-size` for heap size, and `--heap-size-policy` to request a
fixed, growable, or adaptive heap size policy:

```
$ ./pre-inst-env whiffle --parallelism=1 examples/peano-fib.scm 25
121393
```

Again, pass `-o` to write a binary instead of running the compiled file
directly.

Maybe you want to choose a different GC?  Pass `--gc`:

```
$ ./pre-inst-env whiffle --gc=whippet examples/peano-fib.scm 25
121393
```

See `whiffle --help` for more.
