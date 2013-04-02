-- |
-- Module:       $HEADER$
-- Description:  Commonly used lifting operations.
-- Copyright:    (c) 2011 - 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Commonly used lifting operations for 'Either' and 'Maybe'. These data types
-- are used for exception/error which results in quite a lot of boilerplate
-- code.
module Control.Applicative.LiftUtils
    (
    -- * Either

    -- | Deconstructing 'Either' and lifting the result to 'Applicative'
    -- functor.
      fromEitherA

    -- * Maybe

    -- | Deconstructing 'Maybe' and lifting the result to 'Applicative'
    -- functor.
    , fromMaybeA

    -- * Cheet Sheet
    --
    -- | This list provides quick reference of some basic ideas how
    -- 'fromEitherA' and 'fromMaybeA' can be used.  More important then the
    -- code is the type. Code provides proof that function with such type
    -- exists.
    --
    -- Some notes before we start:
    --
    -- * @def@ is a method of @Default@ class from /data-default/ package
    --   <http://hackage.haskell.org/package/data-default>.
    --
    -- * @ErrorT@ can be found, among others, in /transformers/ package
    --   <http://hackage.haskell.org/package/transformers>.
    --
    -- * @MaybeT@ can be found in /transformers/ package
    --   <http://hackage.haskell.org/package/transformers> and few others as
    --   well.
    --
    -- * Whenever there is a 'Monad' constraint, in the code below, you can use
    --   'return' instead of 'pure' without changing class constraints.

    -- ** Either
    --
    -- $eitherCheetSheet

    -- ** Maybe
    --
    -- $maybeCheetSheet

    -- * Reexports
    , module Control.Applicative
    )
    where

import Control.Applicative


-- {{{ Either -----------------------------------------------------------------

-- | Shorthand for:
--
-- @(\\ f y -> case y of {'Left' x -> f x; 'Right' z -> 'pure' z})
--     :: 'Applicative' f => (t -> f a) -> 'Either' t a -> f a@
fromEitherA
    :: (Applicative f)
    => (a -> f b)
    -> Either a b
    -> f b
fromEitherA f x = case x of
    Left y -> f y
    Right z -> pure z
{-# INLINE fromEitherA #-}

-- }}} Either -----------------------------------------------------------------

-- {{{ Maybe ------------------------------------------------------------------

-- | Shorthand for:
--
-- @(\\ x y -> case y of {'Nothing' -> x; 'Just' z -> 'pure' z})
--    :: 'Applicative' f => f a -> 'Maybe' a -> f a@
fromMaybeA
    :: (Applicative f)
    => f a
    -> Maybe a
    -> f a
fromMaybeA x y = case y of
    Nothing -> x
    Just z -> pure z
{-# INLINE fromMaybeA #-}

-- {{{ Maybe ------------------------------------------------------------------

-- {{{ Documentation ----------------------------------------------------------

-- $eitherCheetSheet
--
-- > fromEitherA (const mzero)
-- >    :: (Applicative f, MonadPlus f) => Either a b -> f b
--
-- > (\ f -> fromEitherA (pure . f))
-- >    :: Applicative f => (a -> b) -> Either a b -> f b
--
-- > (\ x -> fromEitherA (\ _ -> pure x))
-- >    :: Applicative f => b -> Either a b -> f b
--
-- > fromEitherA (\ _ -> pure def)
-- >     :: (Applicative f, Default b) => Either a b -> f b
--
-- > (>>= fromEitherA (\ _ -> pure def))
-- >     :: (Applicative m, Default b, Monad m) => m (Either a b) -> m b
--
-- > fromEitherA (\ _ -> pure mempty)
-- >     :: (Applicative f, Monoid b) => Either a b -> f b
--
-- > (>>= fromEitherA (\ _ -> pure mempty))
-- >     :: (Applicative m, Monad m, Monoid b) => m (Either a b) -> m b
--
-- > (\ f -> runErrorT >=> fromEitherA f)
-- >     :: (Applicative m, Monad m)
-- >     => (e -> m a)
-- >     -> ErrorT e m a
-- >     -> m a
--
-- > (\ f -> runErrorT >=> fromEitherA f)
-- >     :: (Applicative m, Monad m)
-- >     => (e -> m a)
-- >     -> ErrorT e m a
-- >     -> m a
--
-- > (\ f -> runErrorT >=> fromEitherA (throw . f))
-- >     :: (Applicative m, Exception e', Monad m)
-- >     => (e -> e')
-- >     -> ErrorT e m a
-- >     -> m a
--
-- > (\ e -> runErrorT >=> fromEitherA (const $ throw e))
-- >     :: (Applicative m, Exception e, Monad m)
-- >     => e
-- >     -> ErrorT e' m a
-- >     -> m a

-- $maybeCheetSheet
--
-- This list provides quick reference of some basic ideas how 'fromMaybeA' can
-- be used.
--
-- > fromMaybeA mzero
-- >     :: (Applicative m, MonadPlus m) => Maybe a -> f a
--
-- > (>>= fromMaybeA mzero)
-- >     :: (Applicative m, MonadPlus m)
-- >     => m (Maybe a)
-- >     -> m a
--
-- > fromMaybeA (pure def)
-- >     :: (Applicative f, Default a) => Maybe a -> f a
--
-- > (>>= fromMaybeA (pure def))
-- >     :: (Applicative m, Default a, Monad m)
-- >     => m (Maybe a)
-- >     -> m a
--
-- > fromMaybeA (pure mempty)
-- >     :: (Applicative f, Monoid a) => Maybe a -> f a
--
-- > (>>= fromMaybeA (pure mempty))
-- >     :: (Applicative m, Monad m, Monoid a)
-- >     => m (Maybe a)
-- >     -> m a
--
-- > fromMaybeA (throw myException) :: Applicative f => Maybe a -> f a
--
-- > (>>= fromMaybeA (throw myException))
-- >     :: (Applicative m, Monad m)
-- >     => m (Maybe a)
-- >     -> m a
--
-- > \ x -> runMaybeT >=> fromMaybeA x
-- >     :: (Applicative m, Monad m) => m a -> MaybeT m a -> m a
--
-- > \ e -> runMaybeT >=> fromMaybeA (throw e)
-- >     :: (Applicative m, Exception e, Monad m) => e -> MaybeT m a -> m a

-- }}} Documentation ----------------------------------------------------------
