-- |
-- Module      :  $HEADER$
-- Description :  Utility functions for Functors
-- Copyright   :  (c) 2011 Peter Trsko
-- License     :  BSD3
--
-- Maintainer  :  dogmat@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility functions for Functors.
--
module Data.Functor.Utils
    (
      (<$>)
    , (<$$>)
    , (<$)
    , ($>)
    , (>$>)
    , (<$<)
    )
    where

-- Module Data.Functor was introduced in base 4.2.0.0 and therefore using this
-- instead.
import Control.Applicative ((<$), (<$>))


-- | Flipped version of '<$>', the naming convention comes from
-- "Control.Applicative" where '<**>' is flipped version of '<*>'.
(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip fmap
infixl 4 <$$>
{-# INLINE (<$$>) #-}

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
infixl 4 $>
{-# INLINE ($>) #-}

-- | Like @\ x f -> f '<*>' 'pure' x@, but does not have 'Applicative'
-- constraint.
--
-- Implemented as: @x <$< f = ('$' x) '<$>' f@.
(<$<) :: (Functor f) => a -> f (a -> b) -> f b
x <$< f = ($ x) `fmap` f
infixl 4 <$<
{-# INLINE (<$<) #-}

-- | Flipped version of ('>$>'). Like @\ f x -> f '<*>' 'pure' x@, but does not
-- have 'Applicative' constraint.
--
-- Example:
--
-- >>> Just (+1) >$> 2
-- 3
--
-- For @instance 'Functor' ((->) r)@ this function behaves as 'flip':
--
-- >>> (-) >$> 1 $ 2
-- 1
(>$>) :: (Functor f) => f (a -> b) -> a -> f b
f >$> x = ($ x) `fmap` f
infixl 4 >$>
{-# INLINE (>$>) #-}
