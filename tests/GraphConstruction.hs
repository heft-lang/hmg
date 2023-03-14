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
    assertHasNoEdges
    assertHasNoClosedEdges

testAddSingleEdge :: IO ()
testAddSingleEdge = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    assertHasEdge s1 Lbl1 s2
    assertHasNoClosedEdges

-- Add multiple edges from s1 to s2 with different labels
testAddMultipleEdges1 :: IO ()
testAddMultipleEdges1 = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    addEdge s1 Lbl2 s2
    assertHasEdge s1 Lbl1 s2
    assertHasEdge s1 Lbl2 s2
    assertHasNoClosedEdges

-- Add edges from s1 to s2 and back
testAddMultipleEdges2 :: IO ()
testAddMultipleEdges2 = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    addEdge s2 Lbl2 s1
    assertHasEdge s1 Lbl1 s2
    assertHasEdge s2 Lbl2 s1
    assertHasNoClosedEdges

tests :: Test
tests = TestList
    [ "testAddSingleScope"    ~: testAddSingleScope
    , "testAddMultipleScopes" ~: testAddMultipleScopes
    , "testAddSingleEdge"     ~: testAddSingleEdge
    , "testAddMultipleEdges1" ~: testAddMultipleEdges1
    , "testAddMultipleEdges2" ~: testAddMultipleEdges2 ]
