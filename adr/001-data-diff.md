# Use Editscript for creating patches at swap!/reset! time

## Status

Accepted

## Context

We need to provide a nicely ergonomic API to programs using Converge,
and we wanted to adhere closely to the Clojure Way (TM).  We decided
to use the Atom API of swap!/reset!/deref.

When altering the value of the ConvergentRef via swap!/reset!, we need
to create a covering set of Ops to describe the change being made.  We
considered the following alternatives:

1. Using `clojure.data/diff` on old-value and new-value, with metadata
   for Id accounting
2. Using [`differ`](https://github.com/Skinney/differ) on old-value
   and new-value, with metadata for Id accounting
3. Using [`editscript`](https://github.com/juji-io/editscript) on
   old-value and new-value, with metadata for Id accounting
4. Implementing the Map/Vector interfaces a la
   [Schism](https://github.com/aredington/schism/blob/master/src/schism/impl/types/nested_map.cljc)
   and do our tracking and accounting within those implementation

## Decision

On initial analysis, it appears that #4 above is flawed, as
implementing our own tracking/accounting nested map/list types
wouldn't maintain the necessary context to translate to an opset.

So that left us with a strategy based on diffing/patching. Based on
analysis in [this blog
post](https://juji.io/blog/comparing-clojure-diff-libraries/), as well
as our own usage testing with `clojure.data/diff`, `differ`, and
`editscript`, we have decided to use
[`editscript`](https://github.com/juji-io/editscript) for creating
opset patches.

## Consequences

TBD
