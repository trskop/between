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
-- top of very basic concept of:
--
-- @f '.' h '.' g@
--
-- where @f@ and @g@ are fixed. It is possible to reduce it to just:
--
-- @(f '.') '.' ('.' g)@
--
-- which is the core pattern used by all functions defined in this module.
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
-- generalization we kept the original @(f '.') '.' ('.' g)@ which we named
-- 'between' or '<@>' in its infix form.
module Data.Function.Between
    (
    -- * Composability
    --
    -- $composability

    -- * Mapping functions for newtypes
    --
    -- $mappingFunctionsForNewtypes

    -- * Between Function Combinator
    --
    -- | Captures common pattern of @\\g -> (f . g . h)@ where @f@ and @h@ are
    -- fixed parameters.
      between
    , (<@>)
    , (<@@>)

    -- ** Derived Combinators
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
"id/between/id"            between id id = id
"id/between"     forall f. between id f  = (. f)
"between/id"     forall f. between f  id = (f .)
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
(<@@>) :: (a -> b) -> (c -> d) -> (b -> c) -> a -> d
(<@@>) = flip between
infixr 8 <@@>
{-# INLINE (<@@>) #-}

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
-- This shows us that it is possible to define @f '<@>' g@ and @h '<@>' i@
-- separately, for reusability, and then compose them.
--
-- The fun doesn't end on functions that take just one parameter, because '<@>'
-- lets you build up things like:
--
-- > (f <@> funOnX) <@> funOnY
-- >     === \g x y -> f (g (funOnX x) (funOnY y))
--
-- As you can se above @g@ is a function that takes two parameters. Now we can
-- define @f '<@>' funOnX@ separately, then when ever we need we can extend it
-- to higher arity function by appending @('<@>' funOnY)@. Special case when
-- @funOnY = funOnX@ is very interesting, in example function
-- @Data.Function.on@ can be defined using 'between' as:
--
-- > on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- > on f g = (id <@> g <@> g) f
-- >     -- or (. g) <@> g
--
-- We can also define function @on3@ that takes function with arity three as
-- its first argument:
--
-- > on3 :: (b -> b -> b -> d) -> (a -> b) -> a -> a -> a -> d
-- > on3 f g = (id <@> g <@> g <@> g) f
-- >     -- or (. g) <@> g <@> g
--
-- Another interesting situation is when @f@ and @g@ in @f '<@>' g@ form an
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
-- When we use @f '<@>' g@ where @f@ and @g@ form an isomorphism of two types,
-- and if @f@ is a constructor and @g@ a selector of newtype, then @f '<@>' g@
-- is a mapping function that allows us to manipulate value wrapped inside a
-- newtype.
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
-- >
-- > mapT3
-- >     :: (a -> b -> c -> d)
-- >     -> T t1 a -> T t2 b -> T t3 c -> T t4 d
-- > mapT3 = mapT2 <@> fromT
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
-- Dually to definition of 'mapT' and 'mapT2' we can also define:
--
-- > comapT :: (T a -> T b) -> a -> b
-- > comapT = fromT <@> T
-- >     -- or T <@@> fromT
-- >
-- > comapT2 :: (T a -> T b -> T c) -> a -> b -> c
-- > comapT2 = fromT <@> T <@> T
-- >     -- or comapT <@> T
-- >     -- or T <@@> T <@@> fromT
-- >     -- or T <@@> comapT
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
