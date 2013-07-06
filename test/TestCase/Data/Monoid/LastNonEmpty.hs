-- |
-- Module:       $HEADER$
-- Description:  Tests for module Data.Monoid.LastNonEmpty.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @Data.Monoid.LastNonEmpty@.
module TestCase.Data.Monoid.LastNonEmpty (tests)
    where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))

import Data.Monoid.LastNonEmpty (LastNonEmpty(..), Monoid(..))


tests :: [Test]
tests =
    [ testGroup "instance (Eq a, Monoid a) => Monoid (LastNonEmpty a)"
        [ testGroup "mempty" test_mempty
        , testGroup "mappend" test_mappend
        ]
    ]

test_mempty :: [Test]
test_mempty =
    [ testCase "LastNonEmpty \"\"" $ LastNonEmpty "" @=? mempty
    , testCase "LastNonEmpty ()" $ LastNonEmpty () @=? mempty
    , testCase "LastNonEmpty [Int]" $ LastNonEmpty ([] :: [Int]) @=? mempty
    ]
{-# ANN test_mempty "HLint: ignore Use camelCase" #-}

test_mappend :: [Test]
test_mappend =
    [ test2 []  [] ([] :: [Int])
    , test2 []  [1] [1 :: Int]
    , test2 [1] []  [1 :: Int]
    , test2 [1] [2] [2 :: Int]
    , test2 [1] [2] [2 :: Int]

    , test3l [1] [2] [3] [3 :: Int]
    , test3l []  [2] [3] [3 :: Int]
    , test3l [1] [2] []  [2 :: Int]

    , test3r [1] [2] [3] [3 :: Int]
    , test3r []  [2] [3] [3 :: Int]
    , test3r [1] [2] []  [2 :: Int]
    ]
  where
    test2 :: (Eq a, Show a, Monoid a) => a -> a -> a -> Test
    test3l, test3r :: (Eq a, Show a, Monoid a) => a -> a -> a -> a -> Test

    test2 x y z =
        testCase (concat [show x, " <> ", show y, " = ", show z])
        $ LastNonEmpty z @=? (LastNonEmpty x `mappend` LastNonEmpty y)

    test3l x y z w =
        testCase (concat
            ["(", show x, " <> ", show y, ") <> ", show z, " = ", show w])
        $ LastNonEmpty w
            @=? ((LastNonEmpty x `mappend` LastNonEmpty y)
                `mappend` LastNonEmpty z)

    test3r x y z w =
        testCase (concat
            [show x, " <> (", show y, " <> ", show z, ") = ", show w])
        $ LastNonEmpty w
            @=? (LastNonEmpty x
                `mappend` (LastNonEmpty y `mappend` LastNonEmpty z))
{-# ANN test_mappend "HLint: ignore Use camelCase" #-}
