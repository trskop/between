-- |
-- Module:       $HEADER$
-- Description:  Various Monad specific utilities.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Variations on bind ('>>=') and 'liftM' that give reader-like feel without
-- the need for monadic transformers.
module Control.Monad.Utils
    ( (>.>), (<.<)
    , (>^>), (<^<)
    , (>|>), (<|<)

    -- * Usage Example
    -- $usageExample
    )
    where

import Control.Monad (liftM)


-- | Shorthand @f x '>>=' g x@.
(>.>) :: Monad m => (a -> m b) -> (a -> b -> m c) -> a -> m c
(>.>) f g x = f x >>= g x
infixl 1 >.>
{-# INLINE (>.>) #-}

-- | Flipped version of '>.>'.
(<.<) :: Monad m => (a -> b -> m c) -> (a -> m b) -> a -> m c
(<.<) = flip (>.>)
infixr 1 <.<
{-# INLINE (<.<) #-}

-- | Shorthand for @f '>.>' flip g@.
(>^>) :: Monad m => (a -> m b) -> (b -> a -> m c) -> a -> m c
f >^> g = f >.> flip g
infixl 1 >^>
{-# INLINE (>^>) #-}

-- | Flipped version of '>^>'.
(<^<) :: Monad m => (b -> a -> m c) -> (a -> m b) -> a -> m c
f <^< g = g >.> flip f
infixr 1 <^<
{-# INLINE (<^<) #-}

-- | Shorthand for @g x \`liftM\` f x@.
(>|>) :: Monad m => (a -> m b) -> (a -> b -> c) -> a -> m c
(>|>) f g x = g x `liftM` f x
infixl 1 >|>
{-# INLINE (>|>) #-}

-- | Flipped version of '>|>'.
(<|<) :: Monad m => (a -> b -> c) -> (a -> m b) -> a -> m c
(<|<) = flip (>|>)
infixr 1 <|<
{-# INLINE (<|<) #-}

-- $usageExample
--
-- > main' :: Config -> IO Result
-- > main' = action1 >.> action2 >|> pureAction3 >.> action4
-- >   where
-- >     action1 :: Config -> IO Result1
-- >     action2 :: Config -> Result1 -> IO Result2
-- >     pureAction3 :: Config -> Result2 -> Result3
-- >     action4 :: Config -> Result3 -> IO Result4
-- >     ...
-- >
-- > main :: IO ()
-- > main = readConfig >>= main'
