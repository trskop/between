-- |
-- Module:       $HEADER$
-- Description:  Function combinator "between" and its variations.
-- Copyright:    (c) 2013, 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- During development it is common occurrence to modify deeply nested
-- structures. One of the best known libraries for this purpose is
-- <http://hackage.haskell.org/package/lens lens>, but it's quite
-- overkill for some purposes.
--
-- This module describes simple and composable combinators that are built on
-- top of very basic concept:
--
-- @f '.' h '.' g@
--
-- Where @f@ and @g@ are fixed. It is possible to reduce it to just:
--
-- @(f '.') '.' ('.' g)@
--
-- Which is the core pattern used by all functions defined in this module.
--
-- Trying to generalize this pattern furhter ends as:
-- @(f \<$\>) '.' (\<$\> g)@, where @\<$\> = 'fmap'@. Other combinations of
-- substituting '.' for 'fmap' will end up less or equally generic. Type of
-- such expression is:
--
-- > \f g -> (f <$>) . (<$> g)
-- >     :: Functor f => (b -> c) -> f a -> (a -> b) -> f c
--
-- Which doesn't give us much more power. Instead of going for such
-- generalization we kept the original @((f '.') '.' ('.' g))@ which we named
-- 'between' or '<@>' in its infix form.
module Data.Function.Between
    (
    -- * Composability
    --
    -- $composability

    -- * Mapping Functions For Newtypes
    --
    -- $mappingFunctionsForNewtypes

    -- * Constructing Lenses
    --
    -- $lenses

    -- * Between Function Combinator
    --
    -- | Captures common pattern of @\\g -> (f . g . h)@ where @f@ and @h@ are
    -- fixed parameters.
      between
    , (<@>)
    , (<@@>)

    -- ** Derived Combinators
    , (<$@>)
    , (<@@$>)
    , between2l
    , between3l
    )
    where

import Prelude (Functor(fmap), (.), ($), flip, id)


-- | Defined as: @\\ f g -> (f .) . (. g)@.
between :: (c -> d) -> (a -> b) -> (b -> c) -> a -> d
between f g = (f .) . (. g)
{-# INLINE between #-}
{-# RULES
"id/between/id"             between id id = id
"id/between"      forall f. between id f  = (. f)
"between/id"      forall f. between f  id = (f .)
  #-}

-- | Infix variant of 'between'.
--
-- Fixity is left associative and set to value 8, which is one less then fixity
-- of function composition ('.').
(<@>) :: (c -> d) -> (a -> b) -> (b -> c) -> a -> d
(<@>) = between
infixl 8 <@>
{-# INLINE (<@>) #-}

-- | Flipped variant of '<@>', i.e. flipped infix variant of 'between'.
--
-- Fixity is right associative and set to value 8, which is one less then
-- fixity of function composition ('.').
(<@@>) :: (a -> b) -> (c -> d) -> (b -> c) -> a -> d
(<@@>) = flip between
infixr 8 <@@>
{-# INLINE (<@@>) #-}

-- | Convenience wrapper for: @\\f g -> 'fmap' f '<@>' g@.
--
-- It allows us to define lenses mostly for pair of functions that form an
-- isomorphism.
--
-- Name of '<$@>' simply says that we apply @\<$\>@ ('fmap') to first (left)
-- argument and then we apply '<@>'.
--
-- of function composition ('.').
(<$@>) :: Functor f => (c -> d) -> (a -> b) -> (b -> f c) -> a -> f d
(<$@>) = between . fmap
infixl 8 <$@>
{-# INLINE (<$@>) #-}

-- | Flipped variant of '<$@>'.
--
-- Name of '<@@$>' simply says that we apply @\<$\>@ ('fmap') to second (right)
-- argument and then we apply '<@@>'.
--
-- Fixity is right associative and set to value 8, which is one less then
-- fixity of function composition ('.').
(<@@$>) :: Functor f => (a -> b) -> (c -> d) -> (b -> f c) -> a -> f d
(<@@$>) = flip $ between . fmap
infixr 8 <@@$>
{-# INLINE (<@@$>) #-}

-- | Defined as: @\\f g -> (f '<@>' g) '<@>' g@.
--
-- Interesting observation:
--
-- > \f g -> between2l id g f === Data.Function.on
between2l :: (c -> d) -> (a -> b) -> (b -> b -> c) -> a -> a -> d
between2l f g = (f `between` g) `between` g
{-# INLINE between2l #-}

-- | Defined as: @\\f g -> ((f '<@>' g) '<@>' g) '<@>' g@.
--
-- Which is equivalent to: @\\f g -> 'between2l' f g '<@>' g@
between3l :: (c -> d) -> (a -> b) -> (b -> b -> b -> c) -> a -> a -> a -> d
between3l f g = ((f `between` g) `between` g) `between` g
{-# INLINE between3l #-}

-- $composability
--
-- > (f . h) <@> (i . g) === (f <@> g) . (h <@> i)
--
-- This shows us that it is possible to define @(f '<@>' g)@ and @(h '<@>' i)@
-- separately, for reusability, and then compose them.
--
-- The fun doesn't end on functions that take just one parameter, because '<@>'
-- lets you build up things like:
--
-- > (f <@> funOnY) <@> funOnX
-- >     === \g x y -> f (g (funOnX x) (funOnY y))
--
-- As you can se above @g@ is a function that takes two parameters. Now we can
-- define @(f '<@>' funOnY)@ separately, then when ever we need we can extend it
-- to higher arity function by appending @('<@>' funOnX)@. Special case when
-- @funOnY = funOnX@ is very interesting, in example function
-- @Data.Function.on@ can be defined using 'between' as:
--
-- > on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- > on f g = (id <@> g <@> g) f
-- >     -- or: ((. g) <@> g) f
--
-- We can also define function @on3@ that takes function with arity three as
-- its first argument:
--
-- > on3 :: (b -> b -> b -> d) -> (a -> b) -> a -> a -> a -> d
-- > on3 f g = (id <@> g <@> g <@> g) f
-- >     -- or: ((. g) <@> g <@> g) f
--
-- If we once again consider generalizing above examples by using three
-- different functions @g1 =\/= g2 =\/= g3@ instead of just one @g@ then we get:
--
-- > on' :: (b -> b1 -> c)
-- >     -> (a2 -> b2)
-- >     -> (a1 -> b1)
-- >     -> a1 -> a2 -> c
-- > on' f g1 g2 = (id <@> g2 <@> g1) f
-- >
-- > on3'
-- >     :: (b1 -> b2 -> b3 -> c)
-- >     -> (a3 -> b3)
-- >     -> (a2 -> b2)
-- >     -> (a1 -> b1)
-- >     -> a1 -> a2 -> a3 -> c
-- > on3' f g1 g2 g3 = (id <@> g3 <@> g2 <@> g1) f
--
-- Which allows us to interpret '<@>' in terms like \"apply this function to the
-- n-th argument before passing it to the function @f@\". We just have to count
-- the arguments backwards. In example if want to apply function @g@ to third
-- argument, but no other then we can use:
--
-- > \g f -> (id <@> g <@> id <@> id) f
-- >     --   ^      ^     ^      ^- Applied to the first argument.
-- >     --   |      |     '- Applied to the second argument.
-- >     --   |      '- Applied to the third argument.
-- >     --   '- Applied to the result.
-- >     :: (a3 -> b3) -> (a1 -> a2 -> b3 -> c) -> a1 -> a2 -> a3 -> c
--
-- Or we can use '<@@>', which is just flipped version of '<@>' and then it
-- would be:
--
-- > \g f -> (id <@@> id <@@> g <@@> id) f
-- >     --   ^       ^       ^      ^- Applied to the result.
-- >     --   |       |       '- Applied to the third argument.
-- >     --   |       '- Applied to the second argument.
-- >     --   '- Applied to the first argument.
-- >     :: (a3 -> b3) -> (a1 -> a2 -> b3 -> c) -> a1 -> a2 -> a3 -> c
--
-- Another interesting situation is when @f@ and @g@ in @(f '<@>' g)@ form an
-- isomorphism. Then we can construct a mapping function that takes function
-- operating on one type and transform it in to a function that operates on a
-- different type. As we shown before it is also possible to map functions with
-- higher arity then one.
--
-- Simplicity of how 'between' combinator can be used to define set of
-- functions by reusing previous definitions makes it also very suitable for
-- usage in TemplateHaskell and generic programming.

-- $mappingFunctionsForNewtypes
--
-- When we use @(f '<@>' g)@ where @f@ and @g@ form an isomorphism of two
-- types, and if @f@ is a constructor and @g@ a selector of newtype, then
-- @(f '<@>' g)@ is a mapping function that allows us to manipulate value
-- wrapped inside a newtype.
--
-- > newtype T t a = T {fromT :: a}
-- >
-- > mapT
-- >     :: (a -> b)
-- >     -> T t a -> T t' b
-- > mapT = T <@> fromT
--
-- Note that @mapT@ above is generalized version of 'fmap' of obvious 'Functor'
-- instance for newtype @T@.
--
-- Interestingly, we can use 'between' to define higher order mapping functions
-- by simple chaining:
--
-- > mapT2
-- >     :: (a -> b -> c)
-- >     -> T t1 a -> T t2 b -> T t3 c
-- > mapT2 = mapT <@> fromT
-- >     -- or: T <@> fromT <@> fromT
-- >     -- or: mapT `between2l` fromT
-- >
-- > mapT3
-- >     :: (a -> b -> c -> d)
-- >     -> T t1 a -> T t2 b -> T t3 c -> T t4 d
-- > mapT3 = mapT2 <@> fromT
-- >     -- or: T <@> fromT <@> fromT <@> fromT
-- >     -- or: mapT `between3l` fromT
--
-- Dually to definition of 'mapT' and 'mapT2' we can also define:
--
-- > comapT :: (T a -> T b) -> a -> b
-- > comapT = fromT <@> T
-- >     -- or: T <@@> fromT
-- >
-- > comapT2 :: (T a -> T b -> T c) -> a -> b -> c
-- > comapT2 = fromT <@> T <@> T
-- >     -- or: comapT <@> T
-- >     -- or: T <@@> T <@@> fromT
-- >     -- or: T <@@> comapT
-- >     -- or: fromT `between2l` T
--
-- Here is another example with a little more complex type wrapped inside a
-- newtype:
--
-- > newtype T e a = T {fromT :: Either e a}
-- >
-- > mapT
-- >     :: (Either e a -> Either e' b)
-- >     -> T e a -> T e' b
-- > mapT = T <@> fromT
-- >
-- > mapT2
-- >     :: (Either e1 a -> Either e2 b -> Either e3 c)
-- >     -> T e1 a -> T e2 b -> T e3 c
-- > mapT2 = mapT <@> fromT
--
-- This last example is typical for monad transformers:
--
-- > newtype ErrorT e m a = ErrorT {runErrorT :: m (Either e a)}
-- >
-- > mapErrorT
-- >     :: (m (Either e a) -> m' (Either e' b))
-- >     -> ErrorT e m a -> ErrorT e' m' b
-- > mapErrorT = ErrorT <@> runErrorT
-- >
-- > mapErrorT2
-- >     :: (m1 (Either e1 a) -> m2 (Either e2 b) -> m3 (Either e3 c))
-- >     -> ErrorT e1 m1 a -> ErrorT e2 m2 b -> ErrorT e3 m3 c
-- > mapErrorT2 = mapErrorT <@> runErrorT

-- $lenses
--
-- Library /lens/ is notorious for its huge list of (mostly transitive)
-- dependencies. However it is easy to define a lot of things without the need
-- to depend on /lens/ directly. This module defines few functions that will
-- make it even easier.
--
-- Lens for a simple newtype:
--
-- > newtype T a = T {fromT :: a}
-- >
-- > t :: Functor f => (a -> f b) -> T a -> f (T b)
-- > t = fmap T <@> fromT
--
-- To simplify things we provide function '<$@>' for it:
--
-- > t :: Functor f => (a -> f b) -> T a -> f (T b)
-- > t = T <$@> fromT
--
-- Definition of lens for generic data type is little more cumbersome:
--
-- > data D a b = D {_x :: a, _y :: b}
-- >
-- > x :: Functor f => (a -> f b) -> D a c -> f (D b c)
-- > -- x :: Lens (D a c) (D b c) a b
-- > x f s = ((\b -> s{_x = b}) <$@> _x) f s
-- >
-- > y :: Functor f => (a -> f b) -> D c a -> f (D c b)
-- > -- y :: Lens (D c a) (D c b) a b
-- > y f s = ((\b -> s{_y = b}) <$@> _y) f s
--
-- Above example shows us that we are able to define function equivalent to
-- @lens@ from /lens/ package as follows:
--
-- > lens
-- >     :: (s -> a)
-- >     -- ^ Selector function.
-- >     -> (s -> b -> t)
-- >     -- ^ Setter function.
-- >     -> (forall f. Functor f => (a -> f b) -> s -> f t)
-- >     -- ^ In /lens/ terms this is @Lens s t a b@
-- > lens get set f s = (set s <$@> get) f s
