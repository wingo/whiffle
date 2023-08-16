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
$ ./pre-inst-env whiffle -e '(lambda () 42)'
42
```

The `pre-inst-env` is for running Whiffle from within the source tree,
which is the only supported way to run it, currently.

You may prefer instead to generate a binary instead of running it
directly:

```
$ ./pre-inst-env whiffle -o foo -e '(lambda () 42)'
$ ./foo
42
```

But, thing is---I don't like to document rough edges, but here we
are---thing is, Whiffle's GC library,
[Whippet](https://github.com/wingo/whippet), includes a number of
concrete garbage collectors, and the default one that Whiffle uses is
`semi`, a simple semi-space collector.  These collectors are
parameterized by
[options](https://github.com/wingo/whippet/blob/main/doc/manual.md#options).
One of the options is `parallelism`, which defaults to the number of
cores available.  But `semi` only supports 1 core, currently; it will
bail if the `parallelism` option is not 1.  So you need to set
`parallelism` to 1.  Whiffle passes options to Whippet via the
`GC_OPTIONS` environment variable, so run like this:

```
$ GC_OPTIONS=parallelism=1 ./out
42
```

Whiffle can compile files instead of expressions:

```
$ GC_OPTIONS=parallelism=1 ./pre-inst-env whiffle examples/peano-fib.scm 25
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
