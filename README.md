Not Found
=========

Over the years of their work Haskellers tend to create a lot of small utilities
and types that they use in various places. It's not uncommon that such
patchworks are published as a small, and also not so small, libraries. This
library is one of those, and we will see in what category it ends up over time.

All functions, types and instances in this library were at some point used in
real world applications. One of the aspects of coding for user applications is
that this library doesn't shy away from sensible language extensions.


External Dependencies
---------------------

* [*comonad*][comonad] (optional) -- Haskell 98 compatible comonads
* [*semigroups*][semigroups] (optional) -- Haskell 98 semigroups


Building options
----------------

* `-fwith-comonad` (enabled by default)
  Adds [*comonad*][comonad] to the list of dependencies and builds Comonad
  instances for various types.
* `-fwith-semigroups` (enabled by default)
  Adds [*semigroups*][semigroups] to the list of dependencies and builds
  Semigroup instances for various types. Implied by `-fwith-comonad`.
* `-fpedantic` (disabled by default)
  Pass additional warning flags including `-Werror` to GHC during compilation.


Contributions
-------------

Pull requests, bug reports and generally contributions in any form are welcome!
Please don't be afraid to contact author using GitHub or by e-mail (see
`.cabal` file for that).

Also try to use `-fpedantic` flag during development and testing.


Lincense
--------

This package is under BSD3 license, see `LICENSE` file for details.

All dependencies are also under the same license, that includes optional
dependencies. See individual packages on hackage for details:

* [base][]
* [comonad][]
* [semigroups][]


[base]:
  http://hackage.haskell.org/package/base/
  "HackageDB: base"

[comonad]:
  http://hackage.haskell.org/package/comonad/
  "HackageDB: comonad"

[semigroups]:
  http://hackage.haskell.org/package/semigroups/
  "HackageDB: semigroups"
