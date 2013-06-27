{-# LANGUAGE CPP #-}
-- |
-- Module:       $HEADER$
-- Description:  Tests for module Data.Functor.FlipT.
-- Copyright:    (c) 2013 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
--
-- Tests for module @Data.Functor.FlipT@.
module TestCase.Data.Functor.FlipT (tests)
    where

import Control.Applicative (Applicative(..))
import Control.Arrow (first)

import Control.Comonad (Comonad(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit ((@=?))

import Data.Functor.FlipT


tests :: [Test]
tests =
    [ testGroup "instance Functor (FlipT Either a)"
        test_instanceFunctorEither
    , testGroup "instance Functor (FlipT (,) a)"
        test_instanceFunctorPair
    , testGroup "instance Monoid a => Applicative (FlipT (,) a)"
        test_instanceApplicativePair
    , testGroup "instance Applicative (FlipT Either a)"
        test_instanceApplicativeEither
#ifdef WITH_COMONAD
    , testGroup "instance Comonad (FlipT (,) a)"
        test_instanceComonadPair
#endif
    , testGroup "mapFlipT"
        test_mapFlipT
    , testGroup "unwrapFlipT"
        test_unwrapFlipT
    , testGroup "flipmap"
        test_flipmap
    ]

test_instanceFunctorEither :: [Test]
test_instanceFunctorEither =
    [ testCase "(+1) `fmap` FlipT (Left 2) = FlipT (Left 3)"
        $ restrict (Left 3) @=? fromFlipT ((+1) `fmap` FlipT (Left 2))
    , testCase "(+1) `fmap` FlipT (Right 2) = FlipT (Right 2)"
        $ restrict (Right 2) @=? fromFlipT ((+1) `fmap` FlipT (Right 2))
    ]
  where
    restrict :: Either Int Int -> Either Int Int
    restrict = id

test_instanceFunctorPair :: [Test]
test_instanceFunctorPair =
    [ testCase "(+1) `fmap` FlipT (2, 'a') = FlipT (3, 'a')"
        $ (3 :: Int, 'a') @=? fromFlipT ((+1) `fmap` FlipT (2, 'a'))
    ]

test_instanceApplicativePair :: [Test]
test_instanceApplicativePair =
    [ testCase "pure 1 = FlipT (1, ()) :: FlipT (Int, ())"
        $ (1 :: Int, ()) @=? fromFlipT (pure 1)
    , testCase "pure False = FlipT (False, []) :: FlipT (Bool, [Int])"
        $ (False, [] :: [Int]) @=? fromFlipT (pure False)
    , testCase
        "FlipT ((+ 1), \"foo\") <*> FlipT (2, \"bar\") = FlipT (3, \"foobar\")"
        $ (3 :: Int, "foobar")
            @=? fromFlipT (FlipT ((+ 1), "foo") <*> FlipT (2, "bar"))
    ]

test_instanceApplicativeEither :: [Test]
test_instanceApplicativeEither =
    [ testCase "pure 1 = FlipT (Left 1) :: FlipT (Either Int Int)"
        $ restrictRight (Left (1 :: Int)) @=? fromFlipT (pure 1)
    , testCase "pure False = FlipT (Left False) :: FlipT (Either Bool Int)"
        $ restrictRight (Left False) @=? fromFlipT (pure False)
    , testCase "FlipT (Left (+ 1)) <*> FlipT (Left 2) = FlipT (Left 3)"
        $ restrictRight (Left (3 :: Int))
            @=? fromFlipT (FlipT (Left (+ 1)) <*> FlipT (Left 2))
    , testCase "FlipT (Left (+ 1)) <*> FlipT (Right 2) = FlipT (Right 2)"
        $ restrict (Right 2)
            @=? fromFlipT (FlipT (Left (+ 1)) <*> FlipT (Right 2))
    , testCase "FlipT (Right 1) <*> FlipT (Left 2) = FlipT (Right 1)"
        $ restrict (Right 1)
            @=? fromFlipT (FlipT (Right 1) <*> FlipT (restrict $ Left 2))
    , testCase "FlipT (Right 1) <*> FlipT (Right 2) = FlipT (Right 1)"
        $ restrict (Right 1)
            @=? fromFlipT (FlipT (Right 1) <*> FlipT (Right 2))
    ]
  where
    restrict :: Either Int Int -> Either Int Int
    restrict = id

    restrictRight :: Either a Int -> Either a Int
    restrictRight = id

#ifdef WITH_COMONAD
test_instanceComonadPair :: [Test]
test_instanceComonadPair =
    [ testCase "extract"
        $ 1 @=? extract (FlipT (1 :: Int, ()))
    , testCase "duplicate"
        $ ((1 :: Int, ()), ())
            @=? (first fromFlipT . fromFlipT . duplicate $ FlipT (1, ()))
    , testCase "extract . duplicate"
        $ (1, ()) @=? (fromFlipT . extract . duplicate $ FlipT (1 :: Int, ()))
    ]
#endif

test_mapFlipT :: [Test]
test_mapFlipT = []

test_unwrapFlipT :: [Test]
test_unwrapFlipT = []

test_flipmap :: [Test]
test_flipmap = []
