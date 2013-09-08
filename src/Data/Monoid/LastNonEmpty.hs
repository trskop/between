{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       $HEADER$
-- Description:  Generalised variant of Data.Monoid.Last.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, DeriveDataTypeable)
--
-- 'LastNonEmpty' is like a generalised version of @Data.Monoid.Last@. For
-- @Maybe a@ values it behaves the same.
module Data.Monoid.LastNonEmpty
    (
    -- * LastNonEmpty
      LastNonEmpty(..)
    , mapLastNonEmpty
    , mapLastNonEmpty2

    -- ** Lenses
    , lastNonEmpty

    -- * Monoid
    , Monoid(..)
#if MIN_VERSION_base(4,5,0)
    , (<>)
#endif
    )
    where

import Data.Monoid (Monoid(..), (<>))
import Data.Data (Data)
import Data.Typeable (Typeable)

#ifdef WITH_SEMIGROUP
import qualified Data.Semigroup as Semigroup (Semigroup(..))
#endif

import Data.Function.Between (between)
import Data.Functor.Utils (iso)


newtype LastNonEmpty a = LastNonEmpty {getLastNonEmpty :: a}
  deriving (Data, Bounded, Eq, Ord, Read, Show, Typeable)

instance (Eq a, Monoid a) => Monoid (LastNonEmpty a) where
    mempty = LastNonEmpty mempty
    {-# INLINEABLE mempty #-}
    x `mappend` y@(LastNonEmpty y')
      | y' == mempty = x
      | otherwise    = y
    {-# INLINEABLE mappend #-}

#ifdef WITH_SEMIGROUP
instance (Eq a, Monoid a) => Semigroup.Semigroup (LastNonEmpty a) where
    (<>) = mappend
    {-# INLINEABLE (<>) #-}
    times1p _ x = x
    {-# INLINEABLE times1p #-}
#endif

instance Functor LastNonEmpty where
    fmap = mapLastNonEmpty

-- | Lift function operating on value wrapped in 'LastNonEmpty' to it's
-- isomorphic counterpart operating on 'LastNonEmpty' wrapped values.
mapLastNonEmpty
    :: (a -> b)
    -> LastNonEmpty a
    -> LastNonEmpty b
mapLastNonEmpty = LastNonEmpty `between` getLastNonEmpty

-- | Variant of 'mapLastNonEmpty' for functions with arity two.
mapLastNonEmpty2
    :: (a -> b -> c)
    -> LastNonEmpty a
    -> LastNonEmpty b
    -> LastNonEmpty c
mapLastNonEmpty2 = mapLastNonEmpty `between` getLastNonEmpty

-- | Lens for 'LastNonEmpty'.
--
-- See /lens/ <http://hackage.haskell.org/package/lens> package for details.
lastNonEmpty
    :: Functor f
    => (a -> f b)
    -> LastNonEmpty a
    -> f (LastNonEmpty b)
lastNonEmpty = iso LastNonEmpty getLastNonEmpty
