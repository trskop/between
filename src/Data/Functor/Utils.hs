{-# LANGUAGE CPP #-}
-- |
-- Module      :  $HEADER$
-- Description :  Utility functions for Functors
-- Copyright   :  (c) 2011, 2013 Peter Trsko
-- License     :  BSD3
--
-- Maintainer  :  peter.trsko@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (CPP)
--
-- Utility functions for Functors.
module Data.Functor.Utils
    (
    -- * Functor
      Functor(..)
#if !MIN_VERSION_base(4,2,0)
    , (<$)
#endif
    , ($>)

    -- ** Infix variations on fmap
    , (<$>)
    , (<$$>)
    , (<<$>>)
    , (<<$$>>)

    -- ** Apply inside functor
    , (<#>)
    , (<##>)

    -- ** Lenses
    --
    -- $lens
    , iso
    , lens
    )
    where

-- Module Data.Functor was introduced in base 4.2.0.0.
#if MIN_VERSION_base(4,2,0)
import Data.Functor (Functor(..), (<$>))
#else
import Control.Applicative ((<$), (<$>))
#endif

import Data.Function.Between (between)


-- | Flipped version of '<$>', the naming convention comes from
-- "Control.Applicative" where '<**>' is flipped version of '<*>'.
(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip fmap
infixl 4 <$$>
{-# INLINE (<$$>) #-}

-- | Flipped version of '<$'.
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
infixl 4 $>
{-# INLINE ($>) #-}

-- | Instead of @\\ x -> f x '<$>' g x@ this function allows to write
-- @f '<<$>>' g@.
(<<$>>) :: Functor f => (a -> b -> c) -> (a -> f b) -> a -> f c
(f <<$>> g) x = f x `fmap` g x
infix 4 <<$>>
{-# INLINE (<<$>>) #-}

-- | Flipped variant of '<<$>>'.
(<<$$>>) :: Functor f => (a -> f b) -> (a -> b -> c) -> a -> f c
(<<$$>>) = flip (<<$>>)
infix 4 <<$$>>
{-# INLINE (<<$$>>) #-}

-- | Like @\\ x f -> f '<*>' 'pure' x@, but does not have 'Applicative'
-- constraint. Flipped version of '<#>'.
--
-- Implemented as: @x '<##>' f = ('$' x) '<$>' f@.
(<##>) :: Functor f => a -> f (a -> b) -> f b
x <##> f = ($ x) `fmap` f
infixl 4 <##>
{-# INLINE (<##>) #-}

-- | Like @\\ f x -> f '<*>' 'pure' x@, but does not have 'Applicative'
-- constraint.
--
-- Implemented as: @f '<#>' x = ('$' x) '<$>' f@.
--
-- Examples:
--
-- >>> Just (+1) <#> 2
-- Just 3
-- >>> [(+1), (*2)] <#> 3
-- [4,6]
--
-- For @instance 'Functor' ((->) r)@ this function behaves as infix version of
-- flip 'flip':
--
-- >>> (-) <#> 1 $ 2
-- 1
(<#>) :: (Functor f) => f (a -> b) -> a -> f b
f <#> x = ($ x) `fmap` f
infixl 4 <#>
{-# INLINE (<#>) #-}

-- | Create /lens/ from @f@ and @g@ that form an isomorphism.
--
-- Defined as: @iso f g = fmap f \`between\` g@
iso :: (Functor f) => (c -> d) -> (a -> b) -> (b -> f c) -> a -> f d
iso = between . fmap
{-# INLINE iso #-}

-- | Construct /lens/ from @r -> (a, a -> r)@-style lens.
lens :: Functor f => (r -> (a, a -> r)) -> (a -> f a) -> r -> f r
lens getSetItem f = setItemIn <<$>> f . getItemFrom
  where
    getItemFrom = fst . getSetItem  -- :: r -> a
    setItemIn = snd . getSetItem    -- :: r -> a -> r
{-# INLINEABLE lens #-}

-- $lens
--
-- For newtypes and data types with single constructor and single attribute it
-- is possible to create lens-like function simply by writing:
--
-- > l = iso constructor selector
--
-- This is due to the fact that constructor and selector together form an
-- isomorphism. The /lens/ package has a lot of sugar for isomorphisms and you
-- should check it out if they come up in your code.
--
-- Example:
--
-- > data Foo a = Foo {fromFoo :: a}
-- >
-- > foo :: Functor f => (a -> f a) -> Foo a -> f (Foo a)
-- > foo = iso Foo fromFoo
--
-- Not so long ago there was a time when people thought that lenses had type
-- @r -> (a, a -> r)@, or variation on that. There is also a lot of old code
-- that it uses it directly or indirectly via some libraries that internally
-- use such definition of lenses. To create new-style /lens/ from this old
-- style you can use 'lens' function:
--
-- > data Foo a = Foo
-- >     { _foo1 :: Int
-- >     , _foo2 :: a
-- >     , _foo3 :: a
-- >     , _foo4 :: a
-- >     }
-- >
-- > foo1 :: Functor f => (Int -> f Int) -> Foo a -> f (Foo a)
-- > foo1 = lens $ \ f@Foo{_foo1 = x} -> (x, \ x' -> f{_foo1 = x'})
--
-- In cases when you need to avoid TemplateHaskell (like on platforms that
-- don't support Haskell byte compilation, yet) these functions might come
-- handy.
--
-- See /lens/ <http://hackage.haskell.org/package/lens> package for details
-- on current standard lenses.
