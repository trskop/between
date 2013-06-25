-- |
-- Module:       $HEADER$
-- Description:  Tests for module Data.Digits.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @Data.Digits@.
module TestCase.Data.Digits (tests)
    where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))

import Data.Digits


tests :: [Test]
tests =
    [ testGroup "numberOfDigitsInBase"
        test_numberOfDigitsInBase
    , testGroup "numberOfDigits"
        test_numberOfDigits
    , testGroup "digitsInBase"
        test_digitsInBase
    , testGroup "digits"
        test_digits
    , testGroup "reverseDigits"
        test_reverseDigits
    , testGroup "fromDigitsInBase"
        test_fromDigitsInBase
    , testGroup "fromDigits"
        test_fromDigits
    , testGroup "genericDigitsInBase"
        test_genericDigitsInBase
    ]

mkTest :: (Eq b, Show a, Show b) => (a -> b, Maybe Int) -> a -> b -> Test
mkTest (f, b) n r =
    testCase (concat $ show n : " --> " : show r
        : maybe [] (\ x -> [" (base ", show x, ")"]) b)
    $ r @=? f n

int :: Int -> Int
int = id

test_numberOfDigitsInBase :: [Test]
test_numberOfDigitsInBase =
    [ mkTest f10 0 0
    , mkTest f10 1 1
    , mkTest f10 (-1) 1
    , mkTest f10 998 3
    , mkTest f10 (-998) 3
    , mkTest f10 1234567890 10
    , mkTest f10 (-1234567890) 10

    , mkTest f16 0 0
    , mkTest f16 1 1
    , mkTest f16 (-1) 1
    , mkTest f16 255 2
    , mkTest f16 (-255) 2
    , mkTest f16 256 3
    , mkTest f16 (-256) 3
    ]
  where
    f = numberOfDigitsInBase
    f10 = (f 10 . int, Just 10)
    f16 = (f 16 . int, Just 16)

test_numberOfDigits :: [Test]
test_numberOfDigits =
    [ mkTest f 0 0
    , mkTest f 1 1
    , mkTest f (-1) 1
    , mkTest f 998 3
    , mkTest f (-998) 3
    , mkTest f 1234567890 10
    , mkTest f (-1234567890) 10
    ]
  where f = (numberOfDigits . int, Nothing)

test_digitsInBase :: [Test]
test_digitsInBase =
    [ mkTest f10 0 []
    , mkTest f10 1 [1]
    , mkTest f10 (-1) [1]
    , mkTest f10 998 [9,9,8]
    , mkTest f10 (-998) [9,9,8]
    , mkTest f10 1234567890 [1,2,3,4,5,6,7,8,9,0]
    , mkTest f10 (-1234567890) [1,2,3,4,5,6,7,8,9,0]

    , mkTest f16 0 []
    , mkTest f16 1 [1]
    , mkTest f16 (-1) [1]
    , mkTest f16 255 [15,15]
    , mkTest f16 (-255) [15,15]
    , mkTest f16 256 [1,0,0]
    , mkTest f16 (-256) [1,0,0]
    ]
  where
    f = digitsInBase
    f10 = (f 10 . int, Just 10)
    f16 = (f 16 . int, Just 16)

test_digits :: [Test]
test_digits =
    [ mkTest f 0 []
    , mkTest f 1 [1]
    , mkTest f (-1) [1]
    , mkTest f 998 [9,9,8]
    , mkTest f (-998) [9,9,8]
    , mkTest f 1234567890 [1,2,3,4,5,6,7,8,9,0]
    , mkTest f (-1234567890) [1,2,3,4,5,6,7,8,9,0]
    ]
  where f = (digits . int, Nothing)

test_reverseDigits :: [Test]
test_reverseDigits =
    [ mkTest f 0 []
    , mkTest f 1 [1]
    , mkTest f (-1) [1]
    , mkTest f 998 [8,9,9]
    , mkTest f (-998) [8,9,9]
    , mkTest f 1234567890 [0,9,8,7,6,5,4,3,2,1]
    , mkTest f (-1234567890) [0,9,8,7,6,5,4,3,2,1]
    ]
  where f = (reverseDigits . int, Nothing)

test_fromDigitsInBase :: [Test]
test_fromDigitsInBase =
    [ mkTest f10 [] 0
    , mkTest f10 [1] 1
    , mkTest f10 [9,9,8] 998
    , mkTest f10 [1,2,3,4,5,6,7,8,9,0] 1234567890
    , mkTest f16 [15,15] 255
    , mkTest f16 [1,0,0] 256
    ]
  where
    f = fromDigitsInBase
    f10 = (int . f 10, Just 10)
    f16 = (int . f 16, Just 16)

test_fromDigits :: [Test]
test_fromDigits =
    [ mkTest f [] 0
    , mkTest f [1] 1
    , mkTest f [9,9,8] 998
    , mkTest f [1,2,3,4,5,6,7,8,9,0] 1234567890
    ]
  where f = (int . fromDigits, Nothing)

test_genericDigitsInBase :: [Test]
test_genericDigitsInBase = []
