{-# LANGUAGE MagicHash #-}
-- |
-- Module:       $HEADER$
-- Description:  Splitting numbers in to digits and vice versa.
-- Copyright:    (c) 2009, 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  non-portable (MagicHash, uses GHC internals)
--
-- Functions for splitting numbers in to digits and summing them back together.
module Data.Digits
    ( numberOfDigitsInBase
    , numberOfDigits
    , genericDigitsInBase
    , digitsInBase
    , digits
    , reverseDigits
    , reverseDigitsInBase
    , fromDigitsInBase
    , fromDigits
    )
    where

import GHC.Base (Int(..), (+#))


-- | Return number of digits number is consisting from in a specified base.
--
-- Zero is an exception, because this function returns zero for it and not one
-- as one might expect. This is due to the fact, that if numbers are
-- represented as a list of digits, it's often desired to interpret zero as an
-- empty list and not list with one element that is zero.
--
-- Some properties that hold:
--
-- > forall b. Integral b =>
-- >     numberOfDigitsInBase b 0 = 0
-- >     numberOfDigitsInBase b (negate n) = numberOfDigits b n
numberOfDigitsInBase :: Integral a => a -> a -> Int
numberOfDigitsInBase _    0 = 0
numberOfDigitsInBase base n = numberOfDigitsInBase# 1#
    $ if n < 0 then negate n else n
  where
    numberOfDigitsInBase# d# m = case m `quot` base of
        0 -> I# d#
        o -> numberOfDigitsInBase# (d# +# 1#) o

-- | Return number of digits in base 10. It's implemented in terms of
-- 'numberOfDigitsInBase' so all it's properties apply also here.
numberOfDigits :: Integral a => a -> Int
numberOfDigits = numberOfDigitsInBase 10

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
genericDigitsInBase _    _ _    0 = id
genericDigitsInBase cons o base n = genericDigitsInBase'
    $ if n < 0 then negate n else n
  where
    genericDigitsInBase' m
      | rest == 0 = cons d
      | otherwise = genericDigitsInBase' rest `o` cons d
      where
        (rest, d) = m `quotRem` base

-- | Split number in to list of digits in specified base.
--
-- Example:
--
-- >>> digitsInBase 10 1234567890
-- [1,2,3,4,5,6,7,8,9,0]
digitsInBase :: Integral a => a -> a -> [a]
digitsInBase base n = genericDigitsInBase (:) (.) base n []

-- | Split number in to list of digits in base 10.
--
-- Example:
--
-- >>> digits 1234567890
-- [1,2,3,4,5,6,7,8,9,0]
digits :: Integral a => a -> [a]
digits = digitsInBase 10

-- | Split number in to list of digits in specified base, but in reverse order.
--
-- Example:
--
-- >>> reverseDigitsInBase 10 1234567890
-- [0,9,8,7,6,5,4,3,2,1]
reverseDigitsInBase :: Integral a => a -> a -> [a]
reverseDigitsInBase base n = genericDigitsInBase (:) (flip (.)) base n []

-- | Split number in to list of digits in base 10, but in reverse order.
--
-- Example:
--
-- >>> reverseDigits 1234567890
-- [0,9,8,7,6,5,4,3,2,1]
reverseDigits :: Integral a => a -> [a]
reverseDigits = reverseDigitsInBase 10

-- | Sum list of digits in specified base.
fromDigitsInBase :: Integral a => a -> [a] -> a
fromDigitsInBase base = foldl ((+) . (base *)) 0

-- | Sum list of digits in base 10.
fromDigits :: Integral a => [a] -> a
fromDigits = fromDigitsInBase 10
