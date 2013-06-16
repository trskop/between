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
