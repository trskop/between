{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:       $HEADER$
-- Description:  Flip arguments of a type.
-- Copyright:    (c) 2013, 2014 Peter Trsko
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
    -- variables. Unfortunately it requires @FlexibleInstances@ and sometimes
    -- also @FlexibleContexts@ language extensions.
    --
    -- Note that @'FlipT' 'Const' b a@ is isomorphic to @'Tagged' b a@ where
    -- 'Tagged' is from @Data.Tagged@ from /tagged/
    -- <http://hackage.haskell.org/package/tagged> package.
      FlipT(FlipT, fromFlipT)
    , flipmap
    , (>$<)
    , (>$$<)

    -- * Utility functions
    --
    -- | Properties that hold:
    --
    -- * @'unwrapFlipT' . 'mapFlipT' = 'id'@
    --
    -- * @'mapFlipT' . 'unwrapFlipT' = 'id'@
    --
    -- Analogic properties hold for 'mapFlipT2' and 'unwrapFlipT2'.
    , mapFlipT
    , mapFlipT2
    , unwrapFlipT
    , unwrapFlipT2
    , withFlip
    , withFlip2

    -- * Lenses
    , flipT
    )
    where

import Control.Applicative (Applicative(..), Const(..))
import Control.Monad.Instances ()
import Data.Monoid (Monoid(..))

#ifdef WITH_COMONAD
import Control.Comonad (Comonad(..))
#endif

import Data.Function.Between (between)
import Data.Functor.Utils (iso)


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

instance Monad (FlipT Either a) where
    return = FlipT . Left

    -- :: FlipT Either a b -> (b -> FlipT Either a c) -> FlipT Either a c
    FlipT (Right x) >>= _ = FlipT (Right x)
    FlipT (Left x)  >>= f = f x

instance Functor (FlipT Const s) where
    -- :: (a -> b) -> FlipT Const s a -> FlipT Const s b
    fmap = mapFlipT . (Const `between` getConst)

instance Applicative (FlipT Const s) where
    -- :: a -> FlipT Const s a
    pure = FlipT . Const

    -- :: FlipT Const s (a -> b) -> FlipT Const s a -> FlipT Const s b
    FlipT (Const f) <*> x = fmap f x

instance Monad (FlipT Const s) where
    -- :: a -> FlipT Const s a
    return = FlipT . Const
    {-# INLINE return #-}

    -- :: FlipT Const s a -> (a -> FlipT Const s b) -> FlipT Const s b
    FlipT (Const x) >>= f = f x
    {-# INLINE (>>=) #-}

    -- :: FlipT Const s a -> FlipT Const s b -> FlipT Const s b
    _ >> x = x
    {-# INLINE (>>) #-}

#ifdef WITH_COMONAD
instance Comonad (FlipT (,) a) where
    duplicate (FlipT p) = FlipT (FlipT p, snd p)
    {-# INLINE duplicate #-}
    extract (FlipT p) = fst p
    {-# INLINE extract #-}

instance Comonad (FlipT Const s) where
    -- :: FlipT Const s a -> FlipT Const s (FlipT Const s a)
    duplicate c = FlipT $ Const c
    {-# INLINE duplicate #-}

    -- :: FlipT Const s a -> a
    extract (FlipT (Const x)) = x
    {-# INLINE extract #-}
#endif

-- | Lift transformation of inner functor in to transofrmation of 'FlipT'
-- functor.
mapFlipT :: (f a b -> g c d) -> FlipT f b a -> FlipT g d c
mapFlipT = FlipT `between` fromFlipT
{-# INLINE mapFlipT #-}

-- | Variant of 'mapFlipT' for functions with arity two.
mapFlipT2
    :: (f1 a1 b1 -> f2 a2 b2 -> f3 a3 b3)
    -> FlipT f1 b1 a1 -> FlipT f2 b2 a2 -> FlipT f3 b3 a3
mapFlipT2 = mapFlipT `between` fromFlipT
{-# INLINE mapFlipT2 #-}

-- | Inverse function to 'mapFlipT'.
unwrapFlipT :: (FlipT f a b -> FlipT g c d) -> f b a -> g d c
unwrapFlipT = fromFlipT `between` FlipT
{-# INLINE unwrapFlipT #-}

-- | Inverse function to 'mapFlipT2'.
unwrapFlipT2
    :: (FlipT f1 b1 a1 -> FlipT f2 b2 a2 -> FlipT f3 b3 a3)
    -> f1 a1 b1 -> f2 a2 b2 -> f3 a3 b3
unwrapFlipT2 = unwrapFlipT `between` FlipT
{-# INLINE unwrapFlipT2 #-}

-- | Take function that expects @'FlipT' f a b@ as its argument and return
-- function that takes unwrapped @f b a@ instead.
--
-- This function can be used in situations when we expect function to use
-- instance for @'FlipT' f a b@ instad of instance for @f b a@, in example:
--
-- > withFlip extract :: Comonad (FlipT f a) => f c a -> c
withFlip :: (FlipT f a b -> c) -> f b a -> c
withFlip = (. FlipT) -- = id `between` FlipT
{-# INLINE withFlip #-}

-- | As 'withFlip', but unwraps first two arguments for specified function.
withFlip2 :: (FlipT f a b -> FlipT g c d -> e) -> f b a -> g d c -> e
withFlip2 = withFlip `between` FlipT
{-# INLINE withFlip2 #-}

-- | Like 'fmap', but uses flipped Functor instance.  Short hand for
-- @'unwrapFlipT' . 'fmap'@.
flipmap :: Functor (FlipT f a) => (b -> c) -> f b a -> f c a
flipmap = unwrapFlipT . fmap
{-# INLINE flipmap #-}

-- | Infix variant of 'flipmap'.
--
-- This colides with:
--
-- > (>$<) :: Contravariant f => (a -> b) -> f b -> f a
--
-- Defined in /contravariant/
-- <http://hackage.haskell.org/package/contravariant> package as of version
-- 0.1.3.
(>$<) :: Functor (FlipT f a) => (b -> c) -> f b a -> f c a
(>$<) = flipmap
infixl 4 >$<
{-# INLINE (>$<) #-}
-- Same fixity as (<$>) = fmap.

-- | Infix variant of 'flipmap' with arguments reversed.
--
-- This colides with:
--
-- > (>$$<) :: Contravariant f => f b -> (a -> b) -> f a
--
-- Defined in /contravariant/
-- <http://hackage.haskell.org/package/contravariant> package as of version
-- 0.1.3.
(>$$<) :: Functor (FlipT f a) => f b a -> (b -> c) -> f c a
(>$$<) = flip flipmap
infixl 4 >$$<
{-# INLINE (>$$<) #-}
-- Same fixity as (<$>) = fmap.

-- | Lens for 'FlipT'. Using type definition from /lens/ package this function
-- would have type:
--
-- > flipT :: Lens (FlipT g b a) (FlipT h d c) (g a b) (h c d)
--
-- See /lens/ <http://hackage.haskell.org/package/lens> package for details.
flipT :: Functor f => (g a b -> f (h c d)) -> FlipT g b a -> f (FlipT h d c)
flipT = iso FlipT fromFlipT
{-# INLINE flipT #-}
