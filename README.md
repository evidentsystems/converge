# Converge

This library provides a convergent reference type for Clojure and
ClojureScript, with support for various pluggable backends
implementing different convergence algorithms. Its local modification
and access API resembles the behavior of an Atom (i.e. `swap!`,
`reset!`, and `deref`).  However, it also has API functions for
accessing and applying patches from the local actor to/from remote
actors, and for a converging merge with a remote convergent ref.

The main guarantee provided by this reference type is that local
swap!/reset! operations and patching/merging between various
distributed actors will converge: that is, two convergent references
with the same underlying log of operations will have the same value,
regardless of the order in which these references received these
operations.

Convergent references and the patches they generate also serialize,
e.g. via Transit/Fressian, for storage and transmission.

## Status

> :warning:

Converge is pre-alpha quality software. Its API and implementation are
changing rapidly. Use at your own risk!

Evident Systems is using Converge in [oNote](https://onote.com) to
support real-time and async/repository-based collaboration.

## Backends

### Editscript Backend

The default backend is based on Editscript diff/snapshot operations,
which are given a total order via Lamport timestamps.  This
implementation is more efficient (in time and space) than the OpSets
backend, and provides the convergence guarantee described above.
However, relative to the OpSets backend, the Editscript algorithm may
provide results that violate end user expectations.  In particular,
removed elements may reappear due to a subsequent update from a remote
actor.

### OpSets backend

The OpSets backend is based on the [OpSets
paper](https://arxiv.org/pdf/1805.04263.pdf) by [Martin
Kleppmann](https://github.com/ept) and co-authors.  It is a
well-designed algorithm whose behavior conforms to end user
expectations, especially in real-time applications, however it is
somewhat less efficient (in time and space) than the Editscript
backend.

## Usage

``` clj
(require '[converge.api :as convergent])

(def c (convergent/ref {:my :value})) ;; Defaults to :editscript backend
;; or to select another backend:
(def c (convergent/ref {:my :value} {:backend :opset}))
@c
; => {:my :value}

(swap! c assoc :another {:nested {:key [1 2 3]}})

@c
; => {:my :value :another {:nested {:key [1 2 3]}}}

;; swap! and reset! must maintain the top-level datatype
;; (which constraint is enforced via the validator)
(reset! c [:whoops])
; throws

(convergent/opset c)
;=> sorted map of id -> op of all historical operations

@c
;=> new value including changes provided in ops (order in which changes applied doesn't matter)

(convergent/peek-patches c)
;=> a patch represnting the first locally-made change

(convergent/pop-patches! c)
;=> removes the first locally-made change from the queue of patches, returning the new queue

(while true
 (if-let [patch (convergent/peek-patches c)]
  (do
   (do-something-with patch) ;; e.g. send to remote actors
   (convergent/pop-patches! c))
  (Thread/sleep 1000)))

;=> remote (i.e. auto-generated actor distinct from that of `c`)
(def remote-c (convergent/ref-from-opset an-opset))
(swap! remote-c assoc :foo :bar)

;; Merge another convergent ref (be careful that each local convergent ref has a unique actor!)
(convergent/merge! c remote-c)

;; Merge a patch
(convergent/merge! c (convergent/peek-patches remote-c))

@c
;=> new value including changes
```

## Development

This library uses a Makefile for development, testing, and CI/CD use.

To install dev dependencies (tested on Mac OS):

``` bash
$ make bootstrap
```

To run a Clojure REPL:

``` bash
$ make clj-dev CLJ_REPL_ALIAS=:cider-nrepl # or your preferred REPL alias
(test) ; to run test suite from REPL
```

To run a ClojureScript REPL (via `shadow-cljs`):

``` bash
$ make cljs-dev
```

Then connect your nREPL client of choice to port 9999.

To run all tests:

``` bash
$ make test
$ make # `test` is also the default target
```

To analyze lines of code:

``` bash
$ make loc
```

## License

Copyright 2020 Evident Systems LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
