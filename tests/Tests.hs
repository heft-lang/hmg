module Main where

import Test.HUnit
import qualified System.Exit as Exit

import qualified RETest as RE
import qualified GraphConstruction as GC
import qualified Resolution as R

test1 :: Test
test1 = TestCase (assertBool "Must be True" True)

tests :: Test
tests = TestList 
    [ "test1" ~: test1
    , RE.tests
    , GC.tests
    , R.tests ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
