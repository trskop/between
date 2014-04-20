{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       $HEADER$
-- Description:  Generalised variant of Data.Monoid.First.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, DeriveDataTypeable)
--
-- 'FirstNonEmpty' is like a generalised version of @Data.Monoid.First@. For
-- @Maybe a@ values it behaves the same.
module Data.Monoid.FirstNonEmpty
    (
    -- * FirstNonEmpty
      FirstNonEmpty(..)
    , mapFirstNonEmpty
    , mapFirstNonEmpty2

    -- ** Lenses
    , firstNonEmpty

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


newtype FirstNonEmpty a = FirstNonEmpty {getFirstNonEmpty :: a}
  deriving (Data, Bounded, Eq, Ord, Read, Show, Typeable)

instance (Eq a, Monoid a) => Monoid (FirstNonEmpty a) where
    mempty = FirstNonEmpty mempty
    {-# INLINEABLE mempty #-}
    x@(FirstNonEmpty x') `mappend` y
      | x' == mempty = y
      | otherwise    = x
    {-# INLINEABLE mappend #-}

#ifdef WITH_SEMIGROUP
instance (Eq a, Monoid a) => Semigroup.Semigroup (FirstNonEmpty a) where
    (<>) = mappend
    {-# INLINEABLE (<>) #-}
    times1p _ x = x
    {-# INLINEABLE times1p #-}
#endif

instance Functor FirstNonEmpty where
    fmap = mapFirstNonEmpty

-- | Lift function operating on value wrapped in 'FirstNonEmpty' to it's
-- isomorphic counterpart operating on 'FirstNonEmpty' wrapped values.
mapFirstNonEmpty
    :: (a -> b)
    -> FirstNonEmpty a
    -> FirstNonEmpty b
mapFirstNonEmpty = FirstNonEmpty `between` getFirstNonEmpty

-- | Variant of 'mapFirstNonEmpty' for functions with arity two.
mapFirstNonEmpty2
    :: (a -> b -> c)
    -> FirstNonEmpty a
    -> FirstNonEmpty b
    -> FirstNonEmpty c
mapFirstNonEmpty2 = mapFirstNonEmpty `between` getFirstNonEmpty

-- | Lens for 'FirstNonEmpty'.
--
-- See /lens/ <http://hackage.haskell.org/package/lens> package for details.
firstNonEmpty
    :: Functor f
    => (a -> f b)
    -> FirstNonEmpty a
    -> f (FirstNonEmpty b)
firstNonEmpty = iso FirstNonEmpty getFirstNonEmpty
