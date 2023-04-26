{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module RETest where

import Test.HUnit
import Data.Regex

-- Derivatives

testDeriveStuck :: IO ()
testDeriveStuck = assertEqual "Derive stuck" Stuck $ derive 1 Stuck

testDeriveEmpty :: IO ()
testDeriveEmpty = assertEqual "Derive empty" Stuck $ derive 1 Empty

testDeriveAtom_Match :: IO ()
testDeriveAtom_Match = assertEqual "Derive atom: match" Empty $ derive 1 (Atom 1)

testDeriveAtom_NoMatch :: IO ()
testDeriveAtom_NoMatch = assertEqual "Derive atom: no match" Stuck $ derive 2 (Atom 1)

testDerivePipe_Left :: IO ()
testDerivePipe_Left = assertEqual "Derive pipe: left match" Empty $ derive 1 $ Pipe (Atom 1) (Atom 2)

testDerivePipe_Right :: IO ()
testDerivePipe_Right = assertEqual "Derive pipe: right match" Empty $ derive 2 $ Pipe (Atom 1) (Atom 2)

testDerivePipe_Both :: IO ()
testDerivePipe_Both = assertEqual "Derive pipe: right match" (Pipe Empty Empty) $ derive 1 $ Pipe (Atom 1) (Atom 1)

testDerivePipe_None :: IO ()
testDerivePipe_None = assertEqual "Derive pipe: none match" Stuck $ derive 2 $ Pipe (Atom 1) (Atom 1)

-- Frontier

-- PossiblyEmpty

-- DefinitelyEmpty

-- Match

testMatchDot :: IO ()
testMatchDot = assertBool "Expected match" $ match [1, 2] $ Dot (Atom 1) (Atom 2)

-- All tests

tests :: Test
tests = TestList
    [ "testDeriveStuck"             ~: testDeriveStuck
    , "testDeriveEmpty  "           ~: testDeriveEmpty
    , "testDeriveAtom_Match"        ~: testDeriveAtom_Match
    , "testDeriveAtom_NoMatch"      ~: testDeriveAtom_NoMatch
    , "testDerivePipe_Left"         ~: testDerivePipe_Left
    , "testDerivePipe_Right"        ~: testDerivePipe_Right
    , "testDerivePipe_Both"         ~: testDerivePipe_Both
    , "testDerivePipe_None"         ~: testDerivePipe_None
    , "testMatchDot"                ~: testMatchDot ]