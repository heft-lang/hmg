{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Resolution where

import Util
import Test.HUnit (assertEqual, Test (TestList), (~:), assertBool)
import Data.Regex
import Control.Monad.IO.Class
import Free.Scope (ScopePath (Start, Step), ResolvedPath (ResolvedPath), lenRPath, PathOrder)

{- default path orders -}

noOrd :: PathOrder l d
noOrd p p' = False

lenOrd :: PathOrder l d
lenOrd p p' = lenRPath p < lenRPath p'

{- Resolution paths -}

testQuerySingleLabel :: IO ()
testQuerySingleLabel = runSGTest $ do
    s1 <- addScope
    addSink s1 Lbl1 Data1
    assertHasSink s1 Lbl1 Data1
    r <- execQuery s1 (Atom Lbl1) noOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 1 $ length r
    assertClosed s1 Lbl1
    assertOpen s1 Lbl2


testQueryMultipleSinks :: IO ()
testQueryMultipleSinks = runSGTest $ do
    s1 <- addScope
    addSink s1 Lbl1 Data1
    addSink s1 Lbl1 Data2
    assertHasSink s1 Lbl1 Data1
    assertHasSink s1 Lbl1 Data2
    r <- execQuery s1 (Atom Lbl1) noOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 2 $ length r
    assertClosed s1 Lbl1
    assertOpen s1 Lbl2


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
    r <- execQuery s1 re noOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 2 $ length r
    liftIO $ assertBool  "Expected local path"     $ ResolvedPath (Start s1) Lbl2 Data1 `elem` r
    liftIO $ assertBool  "Expected remote path"    $ ResolvedPath (Step (Start s1) Lbl1 s2) Lbl2 Data1 `elem` r
    assertClosed s1 Lbl1
    assertClosed s1 Lbl2
    assertClosed s2 Lbl1
    assertClosed s2 Lbl2


testQueryNoValidPath :: IO ()
testQueryNoValidPath = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl2 s2
    addSink s1 Lbl2 Data1
    addSink s2 Lbl2 Data1
    assertHasEdge s1 Lbl2 s2
    assertHasSink s1 Lbl2 Data1
    assertHasSink s2 Lbl2 Data1
    let re = Dot (Atom Lbl1) (Atom Lbl2)
    r <- execQuery s1 re noOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 0 $ length r
    assertClosed s1 Lbl1
    assertOpen s2 Lbl2
    assertOpen s1 Lbl2
    assertOpen s2 Lbl1


testQueryNoMatchData :: IO ()
testQueryNoMatchData = runSGTest $ do
    s1 <- addScope
    addSink s1 Lbl2 Data1
    assertHasSink s1 Lbl2 Data1
    let re = Atom Lbl2
    r <- execQuery s1 re noOrd $ \case Data1 -> False ; _other -> True
    liftIO $ assertEqual "Result count mismatch" 0 $ length r
    assertClosed s1 Lbl2
    assertOpen s1 Lbl1


testQueryCycle :: IO ()
testQueryCycle = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    addEdge s2 Lbl1 s1
    addSink s1 Lbl2 Data1
    assertHasEdge s1 Lbl1 s2
    assertHasEdge s2 Lbl1 s1
    assertHasSink s1 Lbl2 Data1
    let re = Dot (Star $ Atom Lbl1) (Atom Lbl2)
    r <- execQuery s1 re noOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 1 $ length r
    liftIO $ assertBool  "Expected local path"     $ ResolvedPath (Start s1) Lbl2 Data1 `elem` r
    assertClosed s1 Lbl1
    assertClosed s2 Lbl1
    assertClosed s1 Lbl2
    assertClosed s2 Lbl2

testClosedEdgesRetained :: IO ()
testClosedEdgesRetained = runSGTest $ do
    s1 <- addScope
    r <- execQuery @Label @Data s1 (Atom Lbl1) noOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 0 $ length r
    assertClosed s1 Lbl1
    assertOpen s1 Lbl2
    r <- execQuery s1 (Atom Lbl2) noOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 0 $ length r
    assertClosed s1 Lbl1
    assertClosed s1 Lbl2

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
    r <- execQuery s1 re lenOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 1 $ length r
    liftIO $ assertBool  "Expected local path"     $ ResolvedPath (Start s1) Lbl2 Data1 `elem` r

testOrderBySizeMultiple :: IO ()
testOrderBySizeMultiple = runSGTest $ do
    s1 <- addScope
    s2 <- addScope
    addEdge s1 Lbl1 s2
    addSink s1 Lbl2 Data1
    addSink s1 Lbl2 Data2
    addSink s2 Lbl2 Data1
    assertHasEdge s1 Lbl1 s2
    assertHasSink s1 Lbl2 Data1
    assertHasSink s2 Lbl2 Data1
    let re = Dot (Star $ Atom Lbl1) (Atom Lbl2)
    r <- execQuery s1 re lenOrd (const True)
    liftIO $ assertEqual "Result count mismatch" 2 $ length r
    liftIO $ assertBool  "Expected local path 1"   $ ResolvedPath (Start s1) Lbl2 Data1 `elem` r
    liftIO $ assertBool  "Expected local path 2"   $ ResolvedPath (Start s1) Lbl2 Data2 `elem` r

{- Monotonicity -}

testMonotonicity_Edge :: IO ()
testMonotonicity_Edge = expectFailure "Monotonicity invalidated" $ do
    s1 <- addScope
    execQuery @Label @Data s1 (Atom Lbl1) noOrd (const True)
    s2 <- addScope
    addEdge s1 Lbl1 s2

testMonotonicity_Sink :: IO ()
testMonotonicity_Sink = expectFailure "Monotonicity invalidated" $ do
    s1 <- addScope
    execQuery s1 (Atom Lbl1) noOrd (const True)
    addSink s1 Lbl1 Data1

tests :: Test
tests = TestList
    [ "testQuerySingleLabel"    ~: testQuerySingleLabel
    , "testQueryMultipleSinks"  ~: testQueryMultipleSinks
    , "testQueryOverClosure"    ~: testQueryOverClosure
    , "testQueryCycle"          ~: testQueryCycle
    , "testQueryNoValidPath"    ~: testQueryNoValidPath
    , "testQueryNoMatchData"    ~: testQueryNoMatchData
    , "testClosedEdgesRetained" ~: testClosedEdgesRetained
    , "testOrderBySize"         ~: testOrderBySize
    , "testOrderBySizeMultiple" ~: testOrderBySizeMultiple
    , "testMonotonicity_Edge"   ~: testMonotonicity_Edge
    , "testMonotonicity_Sink"   ~: testMonotonicity_Sink ]

