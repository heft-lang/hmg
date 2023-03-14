module Util where

import Text.Printf
import Free.Scope
import Test.HUnit
import Control.Monad (foldM)

-- scopes

assertHasScope :: Graph l d -> Sc -> IO ()
assertHasScope sg s_exp = do
    let s_act = scopes sg
    let msg   = printf "Expected scope %s to be part of graph, but got %s." (show s_exp) (show s_act)
    assertBool msg (s_exp <= s_act)

assertNotHasScope :: Graph l d -> Sc -> IO ()
assertNotHasScope sg s_exp = do
    let s_act = scopes sg
    let msg   = printf "Expected scope %s not to be part of graph, but actually has up to %s." (show s_exp) (show s_act)
    assertBool msg (s_exp > s_act)

-- edges

assertHasEdge :: (Eq l, Eq d, Show l, Show d) => Graph l d -> Sc -> l -> Sc -> IO ()
assertHasEdge sg src lbl tgt = do
    let e_act = entries sg src
    let msg   = printf "Expected edge %s-%s->%s to be part of graph, but got %s." (show src) (show lbl) (show tgt) (show e_act)
    assertBool msg $ (lbl, Left tgt) `elem` e_act


assertScopeHasNoEdges :: (Eq l, Eq d, Show l, Show d) => Graph l d -> Sc -> IO ()
assertScopeHasNoEdges sg s = do
    let e_act = entries sg s
    let msg   = printf "Expected no edges for %s in the graph, but got %s." (show s) (show e_act)
    assertEqual msg [] e_act


assertHasNoEdges :: (Eq l, Eq d, Show l, Show d) => Graph l d -> IO ()
assertHasNoEdges sg = do
    let s = scopes sg
    foldM (\x s' -> assertScopeHasNoEdges sg s') () [1..s]

-- closed edge

assertHasClosedEdge :: (Eq l, Eq d, Show l, Show d) => Graph l d -> Sc -> l -> IO ()
assertHasClosedEdge sg s lbl = do
    let c_act = clos sg s
    let msg   = printf "Expected edge %s-%s->? to be closed in graph, but got %s." (show s) (show lbl) (show c_act)
    assertBool msg $ lbl `elem` c_act


assertScopeHasNoClosedEdges :: (Eq l, Eq d, Show l, Show d) => Graph l d -> Sc -> IO ()
assertScopeHasNoClosedEdges sg s = do
    let c_act = clos sg s
    let msg   = printf "Expected no closed edges for %s in the graph, but got %s." (show s) (show c_act)
    assertEqual msg [] c_act


assertHasNoClosedEdges :: (Eq l, Eq d, Show l, Show d) => Graph l d -> IO ()
assertHasNoClosedEdges sg = do
    let s = scopes sg
    foldM (\x s' -> assertScopeHasNoClosedEdges sg s') () [1..s]
