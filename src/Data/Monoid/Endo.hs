-- |
-- Module:       $HEADER$
-- Description:  Utilities for Endo data type.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Utilities for 'Endo' data type from "Data.Monoid" module.
module Data.Monoid.Endo
    (
    -- * Endo
      Endo(..)
    , runEndo
    , mapEndo
    , liftEndo

    -- * Monoid
    , Monoid(..)
    , (<>)
    )
    where

import Data.Monoid (Endo(..), Monoid(..), (<>))


-- | Transform function wrapped in 'Endo'.
mapEndo :: ((a -> a) -> (b -> b)) -> Endo a -> Endo b
mapEndo f (Endo g) = Endo (f g)
{-# INLINE mapEndo #-}

-- | Apply 'fmap' to function wrapped in 'Endo'. It's a short hand for
-- @'mapEndo' 'fmap'@.
liftEndo :: Functor f => Endo a -> Endo (f a)
liftEndo (Endo f) = Endo (fmap f)
{-# INLINE liftEndo #-}

-- | Flipped version of 'appEndo'.
runEndo :: a -> Endo a -> a
runEndo x (Endo f) = f x
{-# INLINE runEndo #-}
