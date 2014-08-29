Between
=======

[![Hackage](https://budueba.com/hackage/between)][Hackage: between]


Description
-----------

It turns out that this combinator

    f ~@~ g = (f .) . (. g)

is a powerful thing. It was abstracted from following (commonly used)
pattern `f . h . g` where `f` and `g` are fixed.

This library not only define `~@~` combinator, but also some derived
combinators that can help us to easily define a lot of things including
lenses. See [lens package][Hackage: lens] for detais on what lenses are.


Documentation
-------------

Stable releases with API documentation are available on
[Hackage][Hackage: between].


Contributions
-------------

Contributions, pull requests and bug reports are welcome! Please don't be
afraid to contact author using GitHub or by e-mail (see `.cabal` file for
that).


[Hackage: between]:
    https://hackage.haskell.org/package/between
[Hackage: lens]:
    http://hackage.haskell.org/package/lens
