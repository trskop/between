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

    -- ** Examples
    --
    -- $eitherExamples

    -- ** Cheet Sheet
    --
    -- $eitherCheetSheet

    -- * Maybe

    -- | Deconstructing 'Maybe' and lifting the result to 'Applicative'
    -- functor.
    , fromMaybeA

    -- ** Examples
    --
    -- $maybeExamples

    -- ** Cheet Sheet
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

-- $eitherExamples
--
-- TODO

-- $eitherCheetSheet
--
-- TODO

-- $maybeExamples
--
-- TODO

-- $maybeCheetSheet
--
-- > fromMaybeA mzero
-- >     :: (Applicative m, MonadPlus m) => Maybe a -> f a
-- >
-- > (>>= fromMaybeA mzero)
-- >     :: (Applicative m, MonadPlus m)
-- >     => m (Maybe a)
-- >     -> m a
--
-- > -- def is a method of Default class from data-default package.
-- > fromMaybeA (pure def)
-- >     :: (Applicative f, Default a) => Maybe a -> f a
-- >
-- > (>>= fromMaybeA (pure def))
-- >     :: (Applicative m, Default a, Monad m)
-- >     => m (Maybe a)
-- >     -> m a
--
-- > fromMaybeA (pure mempty)
-- >     :: (Applicative f, Monoid a) => Maybe a -> f a
-- >
-- > -- You can use return instead of pure without changing class constraints.
-- > (>>= fromMaybeA (pure mempty))
-- >     :: (Applicative m, Monad m, Monoid a)
-- >     => m (Maybe a)
-- >     -> m a
--
-- > fromMaybeA (throw myException) :: Applicative f => Maybe a -> f a
-- >
-- > (>>= fromMaybeA (throw myException))
-- >     :: (Applicative m, Monad m)
-- >     => m (Maybe a)
-- >     -> m a
--
-- > -- MaybeT can be found in transformers package and few others.
-- > \ x -> runMaybeT >=> fromMaybeA x
-- >     :: (Applicative m, Monad m) => m a -> MaybeT m a -> m a
-- >
-- > \ e -> runMaybeT >=> fromMaybeA (throw e)
-- >     :: (Applicative m, Exception e, Monad m) => e -> MaybeT m a -> m a

-- }}} Documentation ----------------------------------------------------------
