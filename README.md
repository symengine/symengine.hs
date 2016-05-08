# SymEngine-hs

[![Build Status](https://travis-ci.org/symengine/symengine.hs.svg?branch=master)](https://travis-ci.org/symengine/symengine.hs)
[![Test Coverage](https://codeclimate.com/github/symengine/symengine.hs/badges/coverage.svg)](https://codeclimate.com/github/symengine/symengine.hs/coverage)
[![Issue Count](https://codeclimate.com/github/symengine/symengine.hs/badges/issue_count.svg)](https://codeclimate.com/github/symengine/symengine.hs)

Haskell bindings to [SymEngine](https://github.com/symengine/symengine)

# Prerequisites

### [Stack](http://docs.haskellstack.org/en/stable/README/), a Haskell build tool

Stack is a Haskell build tool, which allows for cross-platform, reproducible builds.

The build toolchain of this project depends on `stack`, so please follow the installation
instructions as [outlined here](http://docs.haskellstack.org/en/stable/README/#how-to-install)

### [SymEngine](), the library that this package provides a Haskell interface for

Please go through the `SymEngine` installation instructions, and make sure that the header files
as well as the libraries

* `symengine`
* `gmp`
* `gmpxxx`

Since these are *hard* dependencies for SymEngine-hs to build.

# Getting started

To quickly build and check everything is working, run

```
stack build && stack test
```

All of the test cases should pass with SymEngine

## Playing around in the interpreter

to launch a `GHCi` session, execute the interpreter with

```
stack ghci --ghci-options " -lstdc++ -lgmpxx -lgmp -lsymengine -L/usr/local/lib/"
```


Make sure that you have built `symengine.so` (NOTE: you __need_ the shared object, and not just the library), and
have installed the shared object as well.


Once you are inside `GHCi`, you can execute basic functions such as `basic_const_zero`, `basic_const_one`, etc.


A typical  interpreter session will look like this:

```
GHCi session with Symengine loaded
---

*Symengine Symengine> basic_const_zero
0
*Symengine Symengine> basic_const_zero
0
*Symengine Symengine> basic_const_one
1
*Symengine Symengine> basic_const_minus_one
-1
```

# Things to Do

`[TODO: fill this up]`

# Contributing

`[TODO: fill this up]`

# License

All code is released under the [MIT License](https://github.com/symengine/symengine.hs/blob/master/LICENSE).
