{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Cases(
    bool, maybe', either', op, gadtOp, recTest
) where

import Test.HUnit
import Data.Derive.IsDataCon

derive_is ''Bool

bool :: Test
bool = TestList [
          TestCase (assertEqual "False is False"    (isFalse False) True)
        , TestCase (assertEqual "True is True"      (isTrue True) True)
        , TestCase (assertEqual "False is not True" (isFalse True) False)
        , TestCase (assertEqual "True is not False" (isTrue False) False)
    ]

derive_is ''Maybe

maybe' :: Test
maybe' = TestList [
         TestCase (assertEqual "Nothing is Nothing"  (isNothing Nothing) True)
       , TestCase (assertEqual "Nothing is Nothing"  (isJust (Just ())) True)
       , TestCase (assertEqual "Nothing is not Just" (isNothing (Just ())) False)
       , TestCase (assertEqual "Just is not Nothing" (isJust Nothing) False)]

derive_is ''Either

either' :: Test
either' = TestList [
         TestCase (assertEqual "Left is Left" (isLeft (Left ())) True)
       , TestCase (assertEqual "Right is Right" (isRight (Right ())) True)
       , TestCase (assertEqual "Left is not Right" (isLeft (Right ())) False)
       , TestCase (assertEqual "Right is not Left" (isRight (Left ())) False)]

data (:-:) a b = (:-:) a b | (:=) a b
derive_is ''(:-:)

op :: Test
op = TestList [
         TestCase (assertEqual "ColonMinusColon is ColonMinusColon" (isColonMinusColon ((:-:) () ())) True)
       , TestCase (assertEqual "ColonEqual is ColonEqual" (isColonEqual ( () := ())) True)
       , TestCase (assertEqual "ColonMinusColon is not ColonEqual" (isColonMinusColon ((:=) () ())) False)
       , TestCase (assertEqual "ColonEqual is not ColonMinusColon" (isColonEqual (() :-: ())) False)]


data Gadt a b where
    (:*:), (:=:)  :: a -> b -> Gadt a b

derive_is ''Gadt

gadtOp :: Test
gadtOp = TestList [
         TestCase (assertEqual "ColonStarColon is ColonStarColon" (isColonStarColon ((:*:) () ())) True)
       , TestCase (assertEqual "ColonEqualColon is ColonEqualColon" (isColonEqualColon ( () :=: ())) True)
       , TestCase (assertEqual "ColonStarColon is not ColonEqual" (isColonStarColon ((:=:) () ())) False)
       , TestCase (assertEqual "ColonEqualColon is not ColonStarColon" (isColonEqualColon (() :*: ())) False)]

data Rec a b c = A { getA :: a } | B { getB :: b } | C { getC :: c}

derive_is ''Rec

recTest :: Test
recTest = TestList [
         TestCase (assertEqual "A is A" (isA (A ())) True)
       , TestCase (assertEqual "B is B" (isB (B ())) True)
       , TestCase (assertEqual "C is C" (isC (C ())) True)
       , TestCase (assertEqual "B is not A" (isA (B ())) False)
       , TestCase (assertEqual "C is not A" (isA (C ())) False)
       , TestCase (assertEqual "A is not B" (isB (A ())) False)
       , TestCase (assertEqual "C is not B" (isB (C ())) False)
       , TestCase (assertEqual "A is not C" (isC (A ())) False)
       , TestCase (assertEqual "B is not C" (isC (B ())) False)
  ]