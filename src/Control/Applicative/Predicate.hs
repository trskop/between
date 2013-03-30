-- |
-- Module:       $HEADER$
-- Description:  Prelude predicate functions lifted to Applicative
-- Copyright:    (c) 2012, 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  portable
--
-- Applicative style provides concise way of writing Haskell code that is very
-- readable even without understanding the details. This module provides some
-- "Prelude" predicate functions lifted to 'Applicative'.
module Control.Applicative.Predicate
    (
    -- * Usage

    -- ** Naming conventions
    --
    -- $naming-conventions

    -- ** Usage examples
    --
    -- $usage

    -- * Lifted Operators
    -- ** Lifted Equality Operators
      (.==.)
    , (==.)
    , (.==)

    , (./=.)
    , (/=.)
    , (./=)

    -- ** Lifted Ordering Operators
    , compareA
    , (.<.)
    , (<.)
    , (.>=.)
    , (>=.)
    , (.>.)
    , (>.)
    , (.<=.)
    , (<=.)

    -- ** Lifted Boolean Operators
    , (<||>)
    , (<&&>)

    -- * Other Helper Functions
    , is
    , isNot
    )
  where

import Control.Applicative


-- {{{ Boolean Operators ------------------------------------------------------

-- | Function @('||') :: Bool -> Bool -> Bool@ lifted to 'Applicative'. Has
-- same fixity (@infixr 2@) as ('||').
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
{-# INLINE (<||>) #-}
infixr 2 <||>

-- | Function @('&&') :: Bool -> Bool -> Bool@ lifted to 'Applicative'. Has
-- same fixity (@infixr 3@) as ('&&').
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
{-# INLINE (<&&>) #-}
infixr 3 <&&>

-- }}} Boolean Operators ------------------------------------------------------

-- {{{ Equality ---------------------------------------------------------------

-- | Function @('==') :: 'Eq' a => a -> a -> Bool@ lifted to 'Applicative'. Has
-- same fixity (@infix 4@) as ('==').
(.==.) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(.==.) = liftA2 (==)
{-# INLINE (.==.) #-}
infix 4 .==.

-- | Shorthand for @\ x y -> 'pure' x ('.==.') y@.
(==.) :: (Applicative f, Eq a) => a -> f a -> f Bool
(==.) = (.==.) . pure
{-# INLINE (==.) #-}
infixr 4 ==.

-- | Shorthand for @\ x y -> x ('.==.') 'pure' y@.
(.==) :: (Applicative f, Eq a) => f a -> a -> f Bool
(.==) = flip (==.)
{-# INLINE (.==) #-}
infixr 4 .==

-- | Function @('/=') :: 'Eq' a => a -> a -> Bool@ lifted to 'Applicative'. Has
-- same fixity (@infix 4@) as ('/=').
(./=.) :: (Applicative f, Eq a) => f a -> f a -> f Bool
(./=.) = liftA2 (/=)
{-# INLINE (./=.) #-}
infix 4 ./=.

-- | Shorthand for @\ x y -> 'pure' x ('./=.') y@.
(/=.) :: (Applicative f, Eq a) => a -> f a -> f Bool
(/=.) = (./=.) . pure
{-# INLINE (/=.) #-}
infix 4 /=.

-- | Shorthand for @\ x y -> x ('./=.') 'pure' y@.
(./=) :: (Applicative f, Eq a) => f a -> a -> f Bool
(./=) = flip (/=.)
{-# INLINE (./=) #-}
infix 4 ./=

-- }}} Equality ---------------------------------------------------------------

-- {{{ Ordering ---------------------------------------------------------------

-- | Function @('compare') :: 'Ord' a => a -> a -> 'Ordering'@ lifted to
-- 'Applicative'.
compareA :: (Applicative f, Ord a) => f a -> f a -> f Ordering
compareA = liftA2 compare
{-# INLINE compareA #-}

-- | Function @('<') :: 'Ord' a => a -> a -> 'Bool'@ lifted to 'Applicative'.
-- Has same fixity (@infix 4@) as ('<').
(.<.) :: (Applicative f, Ord a) => f a -> f a -> f Bool
(.<.) = liftA2 (<)
{-# INLINE (.<.) #-}
infix 4 .<.

-- | Shorthand for @\ x y -> 'pure' x ('.<.') y@.
(<.) :: (Applicative f, Ord a) => a -> f a -> f Bool
(<.) = (.<.) . pure
{-# INLINE (<.) #-}
infixr 4 <.

-- | Function @('>=') :: 'Ord' a => a -> a -> 'Bool'@ lifted to 'Applicative'.
-- Has same fixity (@infix 4@) as ('>=').
(.>=.) :: (Applicative f, Ord a) => f a -> f a -> f Bool
(.>=.) = liftA2 (>=)
{-# INLINE (.>=.) #-}
infix 4 .>=.

-- | Shorthand for @\ x y -> 'pure' x ('.>=.') y@.
(>=.) :: (Applicative f, Ord a) => a -> f a -> f Bool
(>=.) = (.>=.) . pure
{-# INLINE (>=.) #-}
infixr 4 >=.

-- | Function @('>') :: 'Ord' a => a -> a -> 'Bool'@ lifted to 'Applicative'.
-- Has same fixity (@infix 4@) as ('>').
(.>.) :: (Applicative f, Ord a) => f a -> f a -> f Bool
(.>.) = liftA2 (>)
{-# INLINE (.>.) #-}
infix 4 .>.

-- | Shorthand for @\ x y -> 'pure' x ('>.') y@.
(>.) :: (Applicative f, Ord a) => a -> f a -> f Bool
(>.) = (.>.) . pure
{-# INLINE (>.) #-}
infixr 4 >.

-- | Function @('<=') :: 'Ord' a => a -> a -> 'Bool'@ lifted to 'Applicative'.
-- Has same fixity (@infix 4@) as ('<=').
(.<=.) :: (Applicative f, Ord a) => f a -> f a -> f Bool
(.<=.) = liftA2 (<=)
{-# INLINE (.<=.) #-}
infix 4 .<=.

-- | Shorthand for @\ x y -> 'pure' x ('.<=.') y@.
(<=.) :: (Applicative f, Ord a) => a -> f a -> f Bool
(<=.) = (.<=.) . pure
{-# INLINE (<=.) #-}
infixr 4 <=.

-- }}} Ordering ---------------------------------------------------------------

-- {{{ Other Helper Functions -------------------------------------------------

-- | Alias for ('==') with the same fixity (@infix 4@).
is :: Eq a => a -> a -> Bool
is = (==)
{-# INLINE is #-}
infix 4 `is`

-- | Alias for ('/=') with the same fixity (@infix 4@).
isNot :: Eq a => a -> a -> Bool
isNot = (/=)
{-# INLINE isNot #-}
infix 4 `isNot`

-- }}} Other Helper Functions -------------------------------------------------

-- {{{ Usage ------------------------------------------------------------------

-- $naming-conventions
--
-- While functions '<&&>' and '<||>' are named consistently with
-- "Control.Applicative" others like '.==.', './=', '>.' and others, use
-- different naming convention. Using same convention for equality and ordering
-- would result in very confusing names like @\<>>@, etc.
--
-- To summarize, when ever possible we use angle brackets to indicate lifted
-- version of the operator surrounded by them, but when it would lead to
-- confusion, then we use dots instead of angle brackets.

-- $usage
--
-- Commonly in byte/word oriented parsers you can find constructs like:
--
-- > isValidChar :: Word8 -> Bool
-- > isValidChar ch = (ch >= 97 && ch <= 122) || ch == 95
-- >     -- (ch >= 'a' && ch <= 'z') || ch == '_'
--
-- Using functions from this module it can be rewritten:
--
-- > isValidChar :: Word8 -> Bool
-- > isValidChar = (>= 97) <&&> (<= 122) <||> is 95
--
-- While above code might look like point-free excercise in reality it's very
-- useful because (i) it is more concise, (ii) very complicated predicates can
-- be expressed and modified faster, and (iii) it can be used instead of lambda
-- functions when passing as a argument to other functions. Example for the
-- (iii):
--
-- > takeWhile ((>= 97) <&&> (<= 122) <||> is 95)
--
-- Instead of:
--
-- > takeWhile (\ ch -> (ch >= 97 && ch <= 122) || ch == 95)
--
-- When dealing with bits we can use simple tricks to simplify predicates:
--
-- > check1, check2 :: Word8 -> Bool
-- >
-- > -- True if at least one of the three most significant bits is set.
-- > check1 = (.&. 112) .> 0
-- >
-- > -- True if three most significant bit and the least most significant bit
-- > -- are set.
-- > check2 = (.&. mask) .== mask <&&> (.&. 1) .> 0
-- >   where mask = 112
--
-- Above examples don't properly manifest why this approach should be used,
-- but they are simple and show how more complicated predicates can be
-- constructed.
--
-- Another common thing is to perform a check that has side effects:
--
-- > import System.Process (rawSystem)
-- >
-- > check :: String -> IO Bool
-- > check command = rawSystem "which" [command] .== ExitSuccess
--
-- Above can be used instead of:
--
-- > check1, check2, check3 :: String -> IO Bool
-- > check1 command = rawSystem "which" [command] >>= (== ExitSuccess)
-- > check2 command = (== ExitSuccess) <$> rawSystem "which" [command]
-- > check3 command = do
-- >     rc <- rawSystem "which" [command]
-- >     return (rc == ExitSuccess)

-- }}} Usage ------------------------------------------------------------------
