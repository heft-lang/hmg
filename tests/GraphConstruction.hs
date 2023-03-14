{-# LANGUAGE TypeApplications #-}
module GraphConstruction where

import Control.Monad.Except (liftEither)
import Test.HUnit

import Free.Scope ( emptyGraph, addScope )

import Util
import Control.Monad.RWS (liftIO)

data Label = Lbl1 | Lbl2 deriving (Show, Eq)
data Data = Data deriving (Show, Eq)



-- Scope addition

testAddSingleScope :: IO ()
testAddSingleScope = runSGTest $ do
    let (s, sg) = addScope $ emptyGraph @Label @Data
    assertHasScope sg 1
    assertNotHasScope sg 2
    assertHasNoEdges sg
    assertHasNoClosedEdges sg

testAddMultipleScopes :: IO ()
testAddMultipleScopes = runSGTest $ do
    let (s, sg) = addScope $ snd $ addScope $ snd $ addScope $ emptyGraph @Label @Data
    assertHasScope sg 1
    assertHasScope sg 2
    assertHasScope sg 3
    assertNotHasScope sg 4
    assertHasNoEdges sg
    assertHasNoClosedEdges sg

testAddSingleEdge :: IO ()
testAddSingleEdge = runSGTest $ do
    let (s1, sg1) = addScope $ emptyGraph @Label @Data
    let (s2, sg2) = addScope sg1
    sg <- addEdge sg2 s1 Lbl1 s2
    assertHasEdge sg s1 Lbl1 s2
    assertHasNoClosedEdges sg

-- Add multiple edges from s1 to s2 with different labels
testAddMultipleEdges1 :: IO ()
testAddMultipleEdges1 = runSGTest $ do
    let (s1, sg1) = addScope $ emptyGraph @Label @Data
    let (s2, sg2) = addScope sg1
    sg3 <- addEdge sg2 s1 Lbl1 s2
    sg  <- addEdge sg3 s1 Lbl2 s2
    assertHasEdge sg s1 Lbl1 s2
    assertHasEdge sg s1 Lbl2 s2
    assertHasNoClosedEdges sg

-- Add edges from s1 to s2 and back
testAddMultipleEdges2 :: IO ()
testAddMultipleEdges2 = runSGTest $ do
    let (s1, sg1) = addScope $ emptyGraph @Label @Data
    let (s2, sg2) = addScope sg1
    sg3 <- addEdge sg2 s1 Lbl1 s2
    sg  <- addEdge sg3 s2 Lbl2 s1
    assertHasEdge sg s1 Lbl1 s2
    assertHasEdge sg s2 Lbl2 s1
    assertHasNoClosedEdges sg

tests :: Test
tests = TestList
    [ "testAddSingleScope"    ~: testAddSingleScope
    , "testAddMultipleScopes" ~: testAddMultipleScopes
    , "testAddSingleEdge"     ~: testAddSingleEdge
    , "testAddMultipleEdges1" ~: testAddMultipleEdges1
    , "testAddMultipleEdges2" ~: testAddMultipleEdges2 ]
