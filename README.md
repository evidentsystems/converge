# Converge

This library provides a convergent reference type for Clojure and
ClojureScript.  Its local modification and access API resembles the
behavior of an Atom (i.e. `swap!`, `reset!`, and `deref`).  However,
it also has API functions for applying patches from the local site or
from remote sites, and for a converging merge with a remote OpSet or
convergent ref.

## Usage

## Development

This library uses a Makefile for development, testing, and CI/CD use.

To install dev dependencies (tested on Mac OS):

``` bash
$ make bootstrap
```

To run a REPL:

``` bash
$ make dev CLJ_REPL_ALIAS=:cider-nrepl # or your preferred REPL alias
(test) ; to run test suite from REPL
```

To run tests from CLI:

``` bash
$ make test
$ make # `test` is also the default target
```

To analyze lines of code:

``` bash
$ make loc
```

## License

Copyright © 2020 Evident Systems LLC
