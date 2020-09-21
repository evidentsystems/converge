# Converge

This library provides a convergent reference type for Clojure and
ClojureScript.  Its local modification and access API resembles the
behavior of an Atom (i.e. `swap!`, `reset!`, and `deref`).  However,
it also has API functions for applying patches from the local actor or
from remote actors, and for a converging merge with a remote
convergent ref.

The current value of the ref is available as usual via `deref` (or
`@`), but the convergent ref itself prints as EDN and serializes for
storage of historical changes to e.g. Transit.

## Usage

``` clj
(require '[converge.api :as converge])

(def c (converge/convergent-ref {:my :value}))
@c
; => {:my :value}

;; swap! and reset! must maintain the top-level datatype
;; (which constraint is enforced via the validator)
(swap! c assoc :another {:nested {:key [1 2 3]}})

@c
; => {:my :value :another {:nested {:key [1 2 3]}}}

(reset! c [:whoops])
; throws

(converge/opset c)
;=> sorted set of all historical operations

(converge/patch! c some-ops)
@c
;=> new value including changes provided in ops (order in which changes applied doesn't matter)

;=> remote (i.e. auto-generated actor distinct from that of `c`)
(def remote-c (converge/ref-from-opset some-ops))

(converge/merge! c remote-c)
@c
;=> new value including changes from the other ref (must be managed by a different actor)
```

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
