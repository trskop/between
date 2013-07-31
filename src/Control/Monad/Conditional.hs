-- |
-- Module:       $HEADER$
-- Description:  Commonly used specialized combinators for conditional
--               execution of monadic expressions.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Commonly used specialized combinators for conditional execution of monadic
-- expressions.
module Control.Monad.Conditional
    (
    -- * Either
      whenLeft
    , whenLeftM
    , whenRight
    , whenRightM

    -- * Maybe
    , whenJust
    , whenJustM
    , whenNothing
    , whenNothingM

    -- * Monoid

    -- | Many useful types are instances of 'Monoid', in example '[]' (that
    -- also imples 'String'), /ByteString/, /Text/, etc.  Using following
    -- functions also allows to get rid of @OverloadedStrings@ extension in
    -- some cases.
    , whenEmpty
    , whenEmptyM
    , unlessEmpty
    , unlessEmptyM
    )
    where

import Data.Monoid (Monoid(mempty))


-- {{{ Either -----------------------------------------------------------------

whenLeft :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft x f = case x of
    Left y -> f y
    _ -> return ()

whenLeftM :: Monad m => m (Either a b) -> (a -> m ()) -> m ()
whenLeftM = liftFstM whenLeft

whenRight :: Monad m => Either a b -> (b -> m ()) -> m ()
whenRight x f = case x of
    Right y -> f y
    _ -> return ()

whenRightM :: Monad m => m (Either a b) -> (b -> m ()) -> m ()
whenRightM = liftFstM whenRight

-- }}} Either -----------------------------------------------------------------

-- {{{ Maybe ------------------------------------------------------------------

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = case x of
    Just y -> f y
    _ -> return ()

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM = liftFstM whenJust

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing x f = case x of
    Nothing -> f
    _ -> return ()

whenNothingM :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM = liftFstM whenNothing

-- }}} Maybe ------------------------------------------------------------------

-- {{{ Monoid -----------------------------------------------------------------

whenEmpty :: (Eq a, Monad m, Monoid a) => a -> m () -> m ()
whenEmpty = flip (`maybeEmpty` \ _ -> return ())

whenEmptyM :: (Eq a, Monad m, Monoid a) => m a -> m () -> m ()
whenEmptyM = liftFstM whenEmpty

unlessEmpty :: (Eq a, Monad m, Monoid a) => a -> (a -> m ()) -> m ()
unlessEmpty = flip (maybeEmpty (return ()))

unlessEmptyM :: (Eq a, Monad m, Monoid a) => m a -> (a -> m ()) -> m ()
unlessEmptyM = liftFstM unlessEmpty

-- }}} Monoid -----------------------------------------------------------------

-- {{{ Helper functions -------------------------------------------------------

-- TODO: Move this function somewhare sensible and export it.
maybeEmpty :: (Eq a, Monoid a) => b -> (a -> b) -> a -> b
maybeEmpty x f y
  | y == mempty = x
  | otherwise = f y
{-# INLINE maybeEmpty #-}

-- Does this function represent some kind of reusable pattern?
liftFstM :: (Monad m) => (a -> b -> m c) -> m a -> b -> m c
liftFstM f x y = x >>= (`f` y)
{-# INLINE liftFstM #-}

-- }}} Helper functions -------------------------------------------------------
