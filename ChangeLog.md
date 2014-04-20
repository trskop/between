# ChangeLog / ReleaseNotes


## Version 0.8.0.0

Release date: **2014-04-20**

* Module `Data.Functor.FlipT`:
  * Monad instance for `FlipT Either a`
  * Functor, Applicative, Monad and Comonad instances for
  `FlipT (Const s)`.
  * New functions: `mapFlipT2`, `unwrapFlipT2`, `withFlip` and
  `withFlip2`.
  * Simple lens for FlipT (in terms of [lens package][]).
* Module `Data.Functor.Utils`:
  * New functions: `(<<$>>)`, `(<<$$>>)`, `iso` and `lens`.
* Introducing module `Data.Function.Between`:
  * Function combinator
  `between :: (c -> d) -> (a -> b) -> (b -> c) -> a -> d`
  implements commonly used pattern `\ f g -> (f .) . (. g)`.
  * Combinators built on top of `between`: `between2l` and
  `between3l`.
* Module `Data.Digits`:
  * Improved error messages.
* Module `Data.Monoid.Endo`:
  * New function `mapEndo2`.
  * Simple lens for Endo (in terms of [lens package][]).
  * Endomorphism type synonym `type E a = a -> a`.
* Modules `Data.Monoid.FirstNonEmpty` and
  `Data.Monoid.LastNonEmpty`:
  * New Functions: `mapFirstNonEmpty`, `mapFirstNonEmpty2`,
  `firstNonEmpty`, `mapLastNonEmpty`, `mapLastNonEmpty2` and
  `lastNonEmpty`.
* Getting rid of HLint warnings.
* Lot of documentation updates.


[lens package]:
  http://hackage.haskell.org/package/lens
  "Hackage: lens: Lenses, Folds and Traversals"
