{-# LANGUAGE MagicHash #-}
-- |
-- Module:       $HEADER$
-- Description:  Splitting numbers in to digits and vice versa.
-- Copyright:    (c) 2009, 2013, 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (MagicHash, uses GHC internals)
--
-- Functions for splitting numbers in to digits and summing them back together.
module Data.Digits
    (
    -- * Counting Digits of a Number
      numberOfDigits
    , numberOfDigitsInBase

    -- * Splitting Number in to Digits
    , digits
    , reverseDigits
    , digitsInBase
    , reverseDigitsInBase
    , genericDigitsInBase

    -- * Joining Digits to a Number
    , fromDigits
    , fromDigitsInBase
    )
    where

import GHC.Base (Int(..), (+#))
import Data.Word (Word, Word8, Word16, Word32, Word64)
    -- Solely required for SPECIALIZE pragmas.


-- | Return number of digits number is consisting from in a specified base.
--
-- Zero is an exception, because this function returns zero for it and not one
-- as it might be expected. This is due to the fact, that if numbers are
-- represented as a list of digits, it's often desired to interpret zero as an
-- empty list and not list with one element that is zero.
--
-- Some properties that hold:
--
-- > forall b. Integral b > 0 =>
-- >     numberOfDigitsInBase b 0 = 0
-- >     numberOfDigitsInBase b n /= 0 when n /= 0
-- >     numberOfDigitsInBase b (negate n) = numberOfDigits b n
numberOfDigitsInBase :: Integral a => a -> a -> Int
numberOfDigitsInBase = numberOfDigitsInBaseImpl "numberOfDigitsInBase"

-- | Implementation behind 'numberOfDigitsInBase' that is parametrized by
-- function name which allows it to print more specific error messages.
--
-- *Should not be exported.*
numberOfDigitsInBaseImpl :: Integral a => String -> a -> a -> Int
numberOfDigitsInBaseImpl _            _    0 = 0
numberOfDigitsInBaseImpl functionName base n
  | base <= 0 = negativeOrZeroBaseError functionName
  | otherwise = numberOfDigitsInBase# 1# $ if n < 0 then negate n else n
  where
    numberOfDigitsInBase# d# m = case m `quot` base of
        0 -> I# d#
        o -> numberOfDigitsInBase# (d# +# 1#) o
{-# INLINE numberOfDigitsInBaseImpl #-}

-- | Return number of digits in base 10. It's implemented in terms of
-- 'numberOfDigitsInBase' so all it's properties apply also here.
numberOfDigits :: Integral a => a -> Int
numberOfDigits = numberOfDigitsInBaseImpl "numberOfDigits" 10
{-# SPECIALIZE numberOfDigits :: Int -> Int #-}
{-# SPECIALIZE numberOfDigits :: Word -> Int #-}
{-# SPECIALIZE numberOfDigits :: Word8 -> Int #-}
{-# SPECIALIZE numberOfDigits :: Word16 -> Int #-}
{-# SPECIALIZE numberOfDigits :: Word32 -> Int #-}
{-# SPECIALIZE numberOfDigits :: Word64 -> Int #-}

-- | Split number in to digits.
--
-- Negative nubers are negated before they are split in to digits. It's similar
-- to how 'numberOfDigitsInBase' handles it.
--
-- This function uses very generic interface so it's possible to use it for any
-- list-like data type. It also works for string/text types like @ByteString@s.
--
-- For an example, of how it can be used, we can look in to definitions of
-- 'digitsInBase' and 'reverseDigitsInBase':
--
-- > digitsInBase :: Integral a => a -> a -> [a]
-- > digitsInBase base n = genericDigitsInBase (:) (.) base n []
-- >
-- > reverseDigitsInBase :: Integral a => a -> a -> [a]
-- > reverseDigitsInBase base n = genericDigitsInBase (:) (flip (.)) base n []
genericDigitsInBase
    :: Integral a
    => (a -> l -> l)
    -- ^ Cons function, for @[a]@ it's @(:) :: a -> [a] -> [a]@.
    -> ((l -> l) -> (l -> l) -> l -> l)
    -- ^ Composition function of list transformation, most commonly it's
    -- either @(.)@ or @flip (.)@. This allows to implement 'digits' and
    -- 'reverseDigits' using one underlying function.
    -> a
    -- ^ Base in which we are operating.
    -> a
    -- ^ Number to be split in to digits.
    -> l -> l
genericDigitsInBase = genericDigitsInBaseImpl "genericDigitsInBase"

-- | Implementation behind 'genericDigitsInBase' that is parametrized by
-- function name which allows it to print more specific error messages.
--
-- *Should not be exported.*
genericDigitsInBaseImpl
    :: Integral a
    => String
    -> (a -> l -> l)
    -> ((l -> l) -> (l -> l) -> l -> l)
    -> a
    -> a
    -> l -> l
genericDigitsInBaseImpl _            _    _ _    0 = id
genericDigitsInBaseImpl functionName cons o base n
  | base <= 0 = negativeOrZeroBaseError functionName
  | otherwise = genericDigitsInBase' $ if n < 0 then negate n else n
  where
    genericDigitsInBase' m
      | rest == 0 = cons d
      | otherwise = genericDigitsInBase' rest `o` cons d
      where
        (rest, d) = m `quotRem` base
{-# INLINE genericDigitsInBaseImpl #-}

-- | Split number in to list of digits in specified base.
--
-- Example:
--
-- >>> digitsInBase 10 1234567890
-- [1,2,3,4,5,6,7,8,9,0]
digitsInBase :: Integral a => a -> a -> [a]
digitsInBase = digitsInBaseImpl "digitsInBase"
{-# SPECIALIZE digitsInBase :: Int -> Int -> [Int] #-}
{-# SPECIALIZE digitsInBase :: Word -> Word -> [Word] #-}
{-# SPECIALIZE digitsInBase :: Word8 -> Word8 -> [Word8] #-}
{-# SPECIALIZE digitsInBase :: Word16 -> Word16 -> [Word16] #-}
{-# SPECIALIZE digitsInBase :: Word32 -> Word32 -> [Word32] #-}
{-# SPECIALIZE digitsInBase :: Word64 -> Word64 -> [Word64] #-}

-- | Implementation behind 'digitsInBase' that is parametrized by function name
-- which allows it to print more specific error messages.
--
-- *Should not be exported.*
digitsInBaseImpl :: Integral a => String -> a -> a -> [a]
digitsInBaseImpl functionName base n =
    genericDigitsInBaseImpl functionName (:) (.) base n []

-- | Split number in to list of digits in base 10.
--
-- Example:
--
-- >>> digits 1234567890
-- [1,2,3,4,5,6,7,8,9,0]
digits :: Integral a => a -> [a]
digits = digitsInBaseImpl "digits" 10
{-# SPECIALIZE digits :: Int -> [Int] #-}
{-# SPECIALIZE digits :: Word -> [Word] #-}
{-# SPECIALIZE digits :: Word8 -> [Word8] #-}
{-# SPECIALIZE digits :: Word16 -> [Word16] #-}
{-# SPECIALIZE digits :: Word32 -> [Word32] #-}
{-# SPECIALIZE digits :: Word64 -> [Word64] #-}

-- | Split number in to list of digits in specified base, but in reverse order.
--
-- Example:
--
-- >>> reverseDigitsInBase 10 1234567890
-- [0,9,8,7,6,5,4,3,2,1]
reverseDigitsInBase :: Integral a => a -> a -> [a]
reverseDigitsInBase = reverseDigitsInBaseImpl "reverseDigitsInBase"
{-# SPECIALIZE reverseDigitsInBase :: Int -> Int -> [Int] #-}
{-# SPECIALIZE reverseDigitsInBase :: Word -> Word -> [Word] #-}
{-# SPECIALIZE reverseDigitsInBase :: Word8 -> Word8 -> [Word8] #-}
{-# SPECIALIZE reverseDigitsInBase :: Word16 -> Word16 -> [Word16] #-}
{-# SPECIALIZE reverseDigitsInBase :: Word32 -> Word32 -> [Word32] #-}
{-# SPECIALIZE reverseDigitsInBase :: Word64 -> Word64 -> [Word64] #-}

-- | Implementation behind 'reverseDigitsInBase' that is parametrized by
-- function name which allows it to print more specific error messages.
--
-- *Should not be exported.*
reverseDigitsInBaseImpl :: Integral a => String -> a -> a -> [a]
reverseDigitsInBaseImpl functionName base n =
    genericDigitsInBaseImpl functionName (:) (flip (.)) base n []

-- | Split number in to list of digits in base 10, but in reverse order.
--
-- Example:
--
-- >>> reverseDigits 1234567890
-- [0,9,8,7,6,5,4,3,2,1]
reverseDigits :: Integral a => a -> [a]
reverseDigits = reverseDigitsInBaseImpl "reverseDigits" 10
{-# SPECIALIZE reverseDigits :: Int -> [Int] #-}
{-# SPECIALIZE reverseDigits :: Word -> [Word] #-}
{-# SPECIALIZE reverseDigits :: Word8 -> [Word8] #-}
{-# SPECIALIZE reverseDigits :: Word16 -> [Word16] #-}
{-# SPECIALIZE reverseDigits :: Word32 -> [Word32] #-}
{-# SPECIALIZE reverseDigits :: Word64 -> [Word64] #-}

-- | Sum list of digits in specified base.
fromDigitsInBase :: Integral a => a -> [a] -> a
fromDigitsInBase base = foldl ((+) . (base *)) 0
{-# SPECIALIZE fromDigitsInBase :: Int -> [Int] -> Int #-}
{-# SPECIALIZE fromDigitsInBase :: Word -> [Word] -> Word #-}
{-# SPECIALIZE fromDigitsInBase :: Word8 -> [Word8] -> Word8 #-}
{-# SPECIALIZE fromDigitsInBase :: Word16 -> [Word16] -> Word16 #-}
{-# SPECIALIZE fromDigitsInBase :: Word32 -> [Word32] -> Word32 #-}
{-# SPECIALIZE fromDigitsInBase :: Word64 -> [Word64] -> Word64 #-}

-- | Sum list of digits in base 10.
fromDigits :: Integral a => [a] -> a
fromDigits = fromDigitsInBase 10
{-# SPECIALIZE fromDigits :: [Int] -> Int #-}
{-# SPECIALIZE fromDigits :: [Word] -> Word #-}
{-# SPECIALIZE fromDigits :: [Word8] -> Word8 #-}
{-# SPECIALIZE fromDigits :: [Word16] -> Word16 #-}
{-# SPECIALIZE fromDigits :: [Word32] -> Word32 #-}
{-# SPECIALIZE fromDigits :: [Word64] -> Word64 #-}

-- | Throw error in case when base value doesn't make sense.
--
-- *Should not be exported.*
negativeOrZeroBaseError :: String -> a
negativeOrZeroBaseError =
    error . (++ ": Negative or zero base doesn't make sense.")
{-# INLINE negativeOrZeroBaseError #-}
