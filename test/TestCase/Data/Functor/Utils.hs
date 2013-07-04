{-# LANGUAGE CPP #-}
-- |
-- Module:       $HEADER$
-- Description:  Tests for module Data.Functor.Utils.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @Data.Functor.Utils@.
module TestCase.Data.Functor.Utils (tests)
    where

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))

import Data.Functor.Utils


tests :: [Test]
tests =
    [ testGroup "<#>"
        test_infixFmapApply
    , testGroup "<##>"
        test_infixFlipFmapApply
    ]

test_infixFmapApply :: [Test]
test_infixFmapApply =
    [ testCase "Just (+1) <#> 2 = Just 3"
        $ Just (3 :: Int) @=? (Just (+1) <#> 2)
    , testCase "[(+1), (*2)] <#> 3 = [4, 6]"
        $ [4, 6 :: Int] @=? ([(+1), (*2)] <#> 3)
    , testCase "(-) <#> 1 $ 2 = 1"
        $ 1 @=? ((-) <#> 1 $ 2 :: Int)
    ]

test_infixFlipFmapApply :: [Test]
test_infixFlipFmapApply =
    [ testCase "2 <##> Just (+1) = Just 3"
        $ Just (3 :: Int) @=? (2 <##> Just (+1))
    , testCase "[(+1), (*2)] <#> 3 = [4, 6]"
        $ [4, 6 :: Int] @=? (3 <##> [(+1), (*2)])
    , testCase "1 <##> (-) $ 2 = 1"
        $ 1 @=? (1 <##> (-) $ 2 :: Int)
    ]
