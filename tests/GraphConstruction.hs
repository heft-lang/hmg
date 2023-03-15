{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module GraphConstruction where

import Test.HUnit
import Free.Scope ( Sc, emptyGraph )
import Util hiding (assertHasEdge, assertHasNoEdges)
import qualified Util as U

data Label = Lbl1 | Lbl2 deriving (Show, Eq)
data Data = Data deriving (Show, Eq)

assertHasEdge :: Sc -> Label -> Sc -> SGTest Label Data ()
assertHasEdge = U.assertHasEdge

assertHasNoEdges :: SGTest Label Data ()
assertHasNoEdges = U.assertHasNoEdges

-- Scope addition

testAddSingleScope :: IO ()
testAddSingleScope = runSGTest $ do
    addScope
    assertHasScope 1
    assertNotHasScope 2
    assertHasNoSinks
    assertHasNoEdges
    assertHasNoClosedEdges

testAddMultipleScopes :: IO ()
testAddMultipleScopes = runSGTest $ do
    addScope
    addScope
    addScope
    assertHasScope 1
    assertHasScope 2
    assertHasScope 3
    assertNotHasScope 4
    assertHasNoSinks
    assertHasNoEdges
    assertHasNoClosedEdges

testAddSingleEdge :: IO ()
testAddSingleEdge = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    assertHasEdge s1 Lbl1 s2
    assertHasNoClosedEdges

testAddMultipleEdges1 :: IO ()
testAddMultipleEdges1 = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    addEdge s1 Lbl2 s2
    assertHasEdge s1 Lbl1 s2
    assertHasEdge s1 Lbl2 s2
    assertHasNoSinks
    assertHasNoClosedEdges

testAddMultipleEdges2 :: IO ()
testAddMultipleEdges2 = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    addEdge s2 Lbl2 s1
    assertHasEdge s1 Lbl1 s2
    assertHasEdge s2 Lbl2 s1
    assertHasNoSinks
    assertHasNoClosedEdges

testAddMultipleEdges_SameLabel_DifferentTarget :: IO ()
testAddMultipleEdges_SameLabel_DifferentTarget = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    s3 <- addScope
    addEdge s1 Lbl1 s2
    addEdge s1 Lbl1 s3
    assertHasEdge s1 Lbl1 s2
    assertHasEdge s1 Lbl1 s3
    assertHasNoSinks
    assertHasNoClosedEdges

-- Sinks

testAddSingleSink :: IO ()
testAddSingleSink = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addSink s1 Lbl1 Data
    assertHasSink s1 Lbl1 Data
    assertHasNoEdges
    assertHasNoClosedEdges

testAddMultipleSinks1 :: IO ()
testAddMultipleSinks1 = runSGTest $ do
    s1 <- addScope
    addSink s1 Lbl1 Data
    addSink s1 Lbl2 Data
    assertHasSink s1 Lbl1 Data
    assertHasSink s1 Lbl2 Data
    assertHasNoEdges
    assertHasNoClosedEdges

testAddMultipleSinks2 :: IO ()
testAddMultipleSinks2 = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addSink s1 Lbl1 Data
    addSink s2 Lbl1 Data
    assertHasSink s1 Lbl1 Data
    assertHasSink s2 Lbl1 Data
    assertHasNoEdges
    assertHasNoClosedEdges

testDuplicateSink :: IO ()
testDuplicateSink = expectFailure "Duplicate sink" $ do
    s1 <- addScope
    addSink s1 Lbl1 Data
    addSink s1 Lbl1 Data
    assertHasSink s1 Lbl1 Data

tests :: Test
tests = TestList
    [ "testAddSingleScope"                              ~: testAddSingleScope
    , "testAddMultipleScopes"                           ~: testAddMultipleScopes
    , "testAddSingleEdge"                               ~: testAddSingleEdge
    , "testAddMultipleEdges1"                           ~: testAddMultipleEdges1
    , "testAddMultipleEdges2"                           ~: testAddMultipleEdges2
    , "testAddSingleSink"                               ~: testAddSingleSink
    , "testAddMultipleSinks1"                           ~: testAddMultipleSinks1
    , "testAddMultipleSinks2"                           ~: testAddMultipleSinks2
    , "testDuplicateSink"                               ~: testDuplicateSink
    , "testAddMultipleEdges_SameLabel_DifferentTarget"  ~: testAddMultipleEdges_SameLabel_DifferentTarget ]
