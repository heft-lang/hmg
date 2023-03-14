{-# LANGUAGE TypeApplications #-}
module GraphConstruction where

import Test.HUnit

import Free.Scope

import Util

data Label = Lbl1 | Lbl2 deriving (Show, Eq)
data Data = Data deriving (Show, Eq)

-- Scope addition

testAddSingleScope :: IO ()
testAddSingleScope = do
    let (s, sg) = addScope $ emptyGraph @Label @Data
    assertHasScope sg 1
    assertNotHasScope sg 2
    assertHasNoEdges sg
    assertHasNoClosedEdges sg

testAddMultipleScopes :: IO ()
testAddMultipleScopes = do
    let (s, sg) = addScope $ snd $ addScope $ snd $ addScope $ emptyGraph @Label @Data
    assertHasScope sg 1
    assertHasScope sg 2
    assertHasScope sg 3
    assertNotHasScope sg 4
    assertHasNoEdges sg
    assertHasNoClosedEdges sg

testAddSingleEdge :: IO ()
testAddSingleEdge = do
    let (s1, sg1) = addScope $ emptyGraph @Label @Data
    let (s2, sg2) = addScope sg1
    let err_or_sg = addEdge sg2 s1 Lbl1 s2
    case err_or_sg of
        Left err -> assertFailure err
        Right sg -> do
            assertHasEdge sg s1 Lbl1 s2
            assertHasNoClosedEdges sg

-- Add multiple edges from s1 to s2 with different labels
testAddMultipleEdges1 :: IO ()
testAddMultipleEdges1 = do
    let (s1, sg1) = addScope $ emptyGraph @Label @Data
    let (s2, sg2) = addScope sg1
    let err_or_sg = addEdge sg2 s1 Lbl1 s2
    case err_or_sg of
        Left err -> assertFailure err
        Right sg3 -> do
            let err_or_sg2 = addEdge sg3 s1 Lbl2 s2
            case err_or_sg2 of
                Left err -> assertFailure err
                Right sg4 -> do
                    assertHasEdge sg4 s1 Lbl1 s2
                    assertHasEdge sg4 s1 Lbl2 s2
                    assertHasNoClosedEdges sg4

-- Add edges from s1 to s2 and back
testAddMultipleEdges2 :: IO ()
testAddMultipleEdges2 = do
    let (s1, sg1) = addScope $ emptyGraph @Label @Data
    let (s2, sg2) = addScope sg1
    let err_or_sg = addEdge sg2 s1 Lbl1 s2
    case err_or_sg of
        Left err -> assertFailure err
        Right sg3 -> do
            let err_or_sg2 = addEdge sg3 s2 Lbl2 s1
            case err_or_sg2 of
                Left err -> assertFailure err
                Right sg4 -> do
                    assertHasEdge sg4 s1 Lbl1 s2
                    assertHasEdge sg4 s2 Lbl2 s1
                    assertHasNoClosedEdges sg4

tests :: Test
tests = TestList
    [ "testAddSingleScope"    ~: testAddSingleScope
    , "testAddMultipleScopes" ~: testAddMultipleScopes
    , "testAddSingleEdge"     ~: testAddSingleEdge
    , "testAddMultipleEdges1" ~: testAddMultipleEdges1
    , "testAddMultipleEdges2" ~: testAddMultipleEdges2 ]
