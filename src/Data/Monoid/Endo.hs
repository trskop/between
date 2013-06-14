{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:       $HEADER$
-- Description:  Utilities for Endo data type.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, FlexibleContexts)
--
-- Utilities for 'Endo' data type from "Data.Monoid" module.
module Data.Monoid.Endo
    (
    -- * Endo
      Endo(..)
    , runEndo
    , mapEndo
    , liftEndo
    , flipLiftEndo

    -- * Monoid
    , Monoid(..)
#if MIN_VERSION_base(4,5,0)
    , (<>)
#endif
    )
    where

import Data.Monoid
    ( Endo(..)
    , Monoid(..)
#if MIN_VERSION_base(4,5,0)
    , (<>)
#endif
    )

import Data.Functor.FlipT (FlipT, flipmap)


-- | Transform function wrapped in 'Endo'.
mapEndo :: ((a -> a) -> (b -> b)) -> Endo a -> Endo b
mapEndo f (Endo g) = Endo (f g)
{-# INLINE mapEndo #-}

-- | Apply 'fmap' to function wrapped in 'Endo'. It's a short hand for
-- @'mapEndo' 'fmap'@.
liftEndo :: Functor f => Endo a -> Endo (f a)
liftEndo (Endo f) = Endo (fmap f)
{-# INLINE liftEndo #-}

-- | Apply 'flipmap' to function wrapped in 'Endo'. It's a short hand for
-- @'mapEndo' 'flipmap'@.
flipLiftEndo :: Functor (FlipT f a) => Endo b -> Endo (f b a)
flipLiftEndo (Endo f) = Endo (flipmap f)
{-# INLINE flipLiftEndo #-}

-- | Flipped version of 'appEndo'.
runEndo :: a -> Endo a -> a
runEndo x (Endo f) = f x
{-# INLINE runEndo #-}
