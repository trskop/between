{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:       $HEADER$
-- Description:  Flip arguments of a type.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (CPP, FlexibleInstances, FlexibleContexts)
--
-- Flip arguments of a type, this enables to create instances like
-- @'Functor' ('FlipT' (,) a)@. Unfortunately it requires few language
-- extensions, namely @FlexibleInstances@ and @FlexibleContexts@.
module Data.Functor.FlipT
    (
    -- * FlipT
    --
    -- | Newtype that allows functor instances with flipped last two type
    -- variables. Unfortunately it requires FlexibleInstances and sometimes
    -- also FlexibleContexts language extensions.
      FlipT(FlipT, fromFlipT)
    , flipmap
    , (>$<)
    , (>$$<)

    -- * Utility functions
    , mapFlipT
    , unwrapFlipT
    )
    where

import Control.Applicative (Applicative(..))
import Control.Monad.Instances ()
import Data.Monoid (Monoid(..))

#ifdef WITH_COMONAD
import Control.Comonad
#endif


-- | Flip last two type variables.
newtype FlipT f a b = FlipT {fromFlipT :: f b a}

instance Functor (FlipT Either a) where
    fmap f (FlipT (Left x))  = FlipT (Left (f x))
    fmap _ (FlipT (Right x)) = FlipT (Right x)
    {-# INLINE fmap #-}

instance Functor (FlipT (,) a) where
    fmap f (FlipT (x, y)) = FlipT (f x, y)
    {-# INLINE fmap #-}

instance Monoid a => Applicative (FlipT (,) a) where
    pure x = FlipT (x, mempty)
    FlipT (f, u) <*> FlipT (x, v) = FlipT (f x, u `mappend` v)

instance Applicative (FlipT Either a) where
    pure = FlipT . Left
    FlipT (Right x) <*> _ = FlipT (Right x)
    FlipT (Left  f) <*> x = fmap f x

#ifdef WITH_COMONAD
instance Comonad (FlipT (,) a) where
    duplicate (FlipT p) = FlipT (FlipT p, snd p)
    {-# INLINE duplicate #-}
    extract (FlipT p) = fst p
    {-# INLINE extract #-}
#endif

mapFlipT :: (f a b -> g c d) -> FlipT f b a -> FlipT g d c
mapFlipT = (FlipT .) . (. fromFlipT)
{-# INLINE mapFlipT #-}

unwrapFlipT :: (FlipT f a b -> FlipT g c d) -> f b a -> g d c
unwrapFlipT = (fromFlipT .) . (. FlipT)
{-# INLINE unwrapFlipT #-}

-- | Like 'fmap', but uses different instance.  Short hand for
-- @'unwrapFlipT' . 'fmap'@.
flipmap :: Functor (FlipT f a) => (b -> c) -> f b a -> f c a
flipmap = unwrapFlipT . fmap
{-# INLINE flipmap #-}

-- | Infix variant of 'flipmap'.
(>$<) :: Functor (FlipT f a) => (b -> c) -> f b a -> f c a
(>$<) = flipmap
infixl 4 >$<
{-# INLINE (>$<) #-}

-- | Infix variant of 'flipmap' with arguments reversed.
(>$$<) :: Functor (FlipT f a) => f b a -> (b -> c) -> f c a
(>$$<) = flip flipmap
infixl 4 >$$<
{-# INLINE (>$$<) #-}
