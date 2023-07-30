# Meta-notes

## How to merge whippet

Whiffle uses [whippet](https://github.com/wingo/whippet-gc/) as the
garbage collector, both as an abstract interface to garbage collection
facilities and as a concrete implementation of a garbage collector.

Whippet is an include-only library, meant to be "vendored" into a
project's source tree.  This is partly because it needs compile-time
specialization against the object representation of a specific program.
Also, this allows compile-time choice of GC algorithm, with inlined fast
paths.  Anyway the way Whiffle incorporates whippet is via git [subtree
merges](https://docs.github.com/en/get-started/using-git/about-git-subtree-merges).

To update Whiffle's copy of whippet, first ensure that you have the
whippet remote:

```
git remote add whippet https://github.com/wingo/whippet-gc
git fetch whippet
```

Then to update:

```
git pull -s subtree whippet main
```
