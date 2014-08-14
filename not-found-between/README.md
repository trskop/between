It turns out that this combinator

    f ~@~ g = (f .) . (. g)

is a powerful thing. It was abstracted from following (commonly used)
pattern `f . h . g` where @f@ and @g@ are fixed.

This library not only defines `~@~` combinator, but also a some derived
combinators that can help us to easily define a lot of things including
lenses. See [lens package](http://hackage.haskell.org/package/lens) for
detais on what lenses are.
