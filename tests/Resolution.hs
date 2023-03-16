module Resolution where

import Util
import Test.HUnit (assertEqual, Test (TestList), (~:), assertBool)
import Data.Regex
import Control.Monad.IO.Class
import Free.Scope (ScopePath (Start, Step), ResolvedPath (ResolvedPath), lenRPath)

{- Resolution paths -}

testQuerySingleLabel :: IO ()
testQuerySingleLabel = runSGTest $ do
    s1 <- addScope
    addSink s1 Lbl1 Data1
    assertHasSink s1 Lbl1 Data1
    r <- execQuery s1 (Atom Lbl1) (\p1 p2 -> False) (const True)
    liftIO $ assertEqual "Result count mismatch" 1 $ length r
    assertClosed s1 Lbl1


testQueryMultipleSinks :: IO ()
testQueryMultipleSinks = runSGTest $ do
    s1 <- addScope
    addSink s1 Lbl1 Data1
    addSink s1 Lbl1 Data2
    assertHasSink s1 Lbl1 Data1
    assertHasSink s1 Lbl1 Data2
    r <- execQuery s1 (Atom Lbl1) (\p1 p2 -> False) (const True)
    liftIO $ assertEqual "Result count mismatch" 2 $ length r


testQueryOverClosure :: IO ()
testQueryOverClosure = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    addSink s1 Lbl2 Data1
    addSink s2 Lbl2 Data1
    assertHasEdge s1 Lbl1 s2
    assertHasSink s1 Lbl2 Data1
    assertHasSink s2 Lbl2 Data1
    let re = Dot (Star $ Atom Lbl1) (Atom Lbl2)
    r <- execQuery s1 re (\p1 p2 -> False) (const True)
    liftIO $ assertEqual "Result count mismatch" 2 $ length r
    liftIO $ assertBool  "Expected local path"     $ ResolvedPath (Start s1) Lbl2 Data1 `elem` r
    liftIO $ assertBool  "Expected remote path"    $ ResolvedPath (Step (Start s1) Lbl1 s2) Lbl2 Data1 `elem` r

{- Path ordering -}

testOrderBySize :: IO ()
testOrderBySize = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    addSink s1 Lbl2 Data1
    addSink s2 Lbl2 Data1
    assertHasEdge s1 Lbl1 s2
    assertHasSink s1 Lbl2 Data1
    assertHasSink s2 Lbl2 Data1
    let re = Dot (Star $ Atom Lbl1) (Atom Lbl2)
    r <- execQuery s1 re (\p1 p2 -> lenRPath p1 < lenRPath p2) (const True)
    liftIO $ assertEqual "Result count mismatch" 1 $ length r
    liftIO $ assertBool  "Expected local path"     $ ResolvedPath (Start s1) Lbl2 Data1 `elem` r


tests :: Test
tests = TestList
    [ "testQuerySingleLabel"   ~: testQuerySingleLabel
    , "testQueryMultipleSinks" ~: testQueryMultipleSinks
    , "testQueryOverClosure"   ~: testQueryOverClosure
    , "testOrderBySize"        ~: testOrderBySize ]

