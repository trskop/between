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
    , whenLeftM
    , whenNothing
    , whenNothingM

    -- * Monoid

    -- | Many useful types are instances of 'Monoid', in example '[]' (that
    -- also imples 'String'), /ByteString/, /Text/, etc.  Using following
    -- functions also allows to get rid of @OverloadedStrings@ extension in
    -- some cases.
    , whenEmpty
    , whenEmptyM
    )
    where

import Data.Monoid (Monoid(mempty))


-- {{{ Either -----------------------------------------------------------------

whenLeft :: Monad m => Either a b -> (a -> m ()) -> m ()
whenLeft x f = case x of
    Left y -> f y
    _ -> return ()

whenLeftM :: Monad m => m (Either a b) -> (a -> m ()) -> m ()
whenLeftM x f = x >>= (`whenLeft` f)

whenRight :: Monad m => Either a b -> (b -> m ()) -> m ()
whenRight x f = case x of
    Right y -> f y
    _ -> return ()

whenRightM :: Monad m => m (Either a b) -> (b -> m ()) -> m ()
whenRightM x f = x >>= (`whenRight` f)

-- }}} Either -----------------------------------------------------------------

-- {{{ Maybe ------------------------------------------------------------------

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = case x of
    Just y -> f y
    _ -> return ()

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM x f = x >>= (`whenJust` f)

whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing x f = case x of
    Nothing -> f
    _ -> return ()

whenNothingM :: Monad m => m (Maybe a) -> m () -> m ()
whenNothingM x m = x >>= (`whenNothing` m)

-- }}} Maybe ------------------------------------------------------------------

-- {{{ Monoid -----------------------------------------------------------------

whenEmpty :: (Eq a, Monad m, Monoid a) => a -> m () -> m ()
whenEmpty x f
  | x == mempty = f
  | otherwise = return ()

whenEmptyM :: (Eq a, Monad m, Monoid a) => m a -> m () -> m ()
whenEmptyM x f = (x >>= (`whenEmpty` f))

unlessEmpty :: (Eq a, Monad m, Monoid a) => a -> (a -> m ()) -> m ()
unlessEmpty x f
  | x == mempty = return ()
  | otherwise = f x

unlessEmptyM :: (Eq a, Monad m, Monoid a) => m a -> (a -> m ()) -> m ()
unlessEmptyM x f = (x >>= (`unlessEmpty` f))

-- }}} Monoid -----------------------------------------------------------------
