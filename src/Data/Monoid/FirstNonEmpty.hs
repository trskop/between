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
