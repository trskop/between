Not Found
=========

Over the years of their work Haskellers tend to create a lot of small utilities
and types that they use in various places. It's not uncommon that such
patchworks are published as a small, and also not so small, libraries. This
library is one of those, and we will see in what category it ends up over time.

All functions, types and instances in this library were at some point used in
real world applications. One of the aspects of coding for user applications is
that this library doesn't shy away from sensible language extensions.


Building options
----------------

* `-fwith-comonad` (enabled by default)
  Adds comonad to the list of dependencies and builds Comonad instances for
  various types.
* `-fwith-semigroups` (enabled by default)
  Adds semigroups to the list of dependencies and builds Semigroup instances
  for various types. Implied by `-fwith-comonad`.
* `-fpedantic` (disabled by default)
  Pass additional warning flags including `-Werror` to GHC during compilation.
