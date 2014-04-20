-- |
-- Module:       $HEADER$
-- Description:  Tests for module Data.Monoid.FirstNonEmpty.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @Data.Monoid.FirstNonEmpty@.
module TestCase.Data.Monoid.FirstNonEmpty (tests)
    where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))

import Data.Monoid.FirstNonEmpty (FirstNonEmpty(..), Monoid(..))


tests :: [Test]
tests =
    [ testGroup "instance (Eq a, Monoid a) => Monoid (FirstNonEmpty a)"
        [ testGroup "mempty" test_mempty
        , testGroup "mappend" test_mappend
        ]
    ]

test_mempty :: [Test]
test_mempty =
    [ testCase "FirstNonEmpty \"\"" $ FirstNonEmpty "" @=? mempty
    , testCase "FirstNonEmpty ()" $ FirstNonEmpty () @=? mempty
    , testCase "FirstNonEmpty [Int]" $ FirstNonEmpty ([] :: [Int]) @=? mempty
    ]
{-# ANN test_mempty "HLint: ignore Use camelCase" #-}

test_mappend :: [Test]
test_mappend =
    [ test2 []  [] ([] :: [Int])
    , test2 []  [1] [1 :: Int]
    , test2 [1] []  [1 :: Int]
    , test2 [1] [2] [1 :: Int]
    , test2 [1] [2] [1 :: Int]

    , test3l [1] [2] [3] [1 :: Int]
    , test3l []  [2] [3] [2 :: Int]
    , test3l [1] [2] []  [1 :: Int]

    , test3r [1] [2] [3] [1 :: Int]
    , test3r []  [2] [3] [2 :: Int]
    , test3r [1] [2] []  [1 :: Int]
    ]
  where
    test2 :: (Eq a, Show a, Monoid a) => a -> a -> a -> Test
    test3l, test3r :: (Eq a, Show a, Monoid a) => a -> a -> a -> a -> Test

    test2 x y z =
        testCase (concat [show x, " <> ", show y, " = ", show z])
        $ FirstNonEmpty z @=? (FirstNonEmpty x `mappend` FirstNonEmpty y)

    test3l x y z w =
        testCase (concat
            ["(", show x, " <> ", show y, ") <> ", show z, " = ", show w])
        $ FirstNonEmpty w
            @=? ((FirstNonEmpty x `mappend` FirstNonEmpty y)
                `mappend` FirstNonEmpty z)

    test3r x y z w =
        testCase (concat
            [show x, " <> (", show y, " <> ", show z, ") = ", show w])
        $ FirstNonEmpty w
            @=? (FirstNonEmpty x
                `mappend` (FirstNonEmpty y `mappend` FirstNonEmpty z))
{-# ANN test_mappend "HLint: ignore Use camelCase" #-}
