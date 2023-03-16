module Util where

import Data.Tuple (swap)
import Text.Printf
import Control.Monad.Except
import Free.Scope ( Graph(scopes, entries, clos), Sc, emptyGraph, sinksOf, edgesOf, ResolvedPath )
import qualified Free.Scope as FS
import Test.HUnit hiding (Path)
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy
import qualified Free.Scope as Fs
import Data.Regex
import Data.Either

-- Copied from higher versions of `transformers`
modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \ s -> do
    s' <- f s
    return ((), s')

{- Define SGTest which has the scope graph as implicit state + support for failure -}
type SGTest l d v = StateT (Graph l d) (ExceptT String IO) v

runSGTest :: SGTest l d () -> IO ()
runSGTest test = runExceptT (evalStateT test emptyGraph) >>= either assertFailure return

expectFailure :: String -> SGTest l d () -> IO ()
expectFailure msg test = runExceptT (evalStateT test emptyGraph) >>= either (const $ return ()) (const $ assertFailure msg)

addScope :: SGTest l d Sc
addScope = state FS.addScope

addEdge :: (Eq l, Show l) => Sc -> l -> Sc -> SGTest l d ()
addEdge s l s' = modifyM $ \g -> liftEither $ FS.addEdge g s l s'

addSink :: (Eq d, Show d,
            Eq l, Show l)
            => Sc -> l -> d -> SGTest l d ()
addSink s l d = modifyM $ \g -> liftEither $ FS.addSink g s l d

execQuery :: ( Show d , Show l , Eq l, Eq d )
          => Sc
          -> RE l
          -> (ResolvedPath l d -> ResolvedPath l d -> Bool)
          -> (d -> Bool)
          -> SGTest l d [ResolvedPath l d]
execQuery sc re po ad = state $ \g -> swap $ FS.execQuery g sc re po ad


{- Scope Graph Parameters -}

data Label = Lbl1  | Lbl2  deriving (Show, Eq)
data Data  = Data1 | Data2 deriving (Show, Eq)

{- Assertions -}

-- Applies `f` to all scopes in the state
iterateScopesIO :: (Eq l, Eq d, Show l, Show d) => (Sc -> SGTest l d ()) -> SGTest l d ()
iterateScopesIO f = do
    g <- get
    let s = scopes g
    foldM (const f) () [1..s]

-- scopes

threadState :: s -> IO () -> ExceptT String IO ((), s)
threadState g m = liftIO $ m >>= \v -> return (v, g)

asserts :: (Graph l d -> IO ()) -> SGTest l d ()
asserts f = modifyM $ \g -> liftIO $ f g >> return g

assertHasScope' :: Sc -> SGTest l d ()
assertHasScope' s = asserts $ \g -> do
    assertBool "" True

assertHasScope :: Sc -> SGTest l d ()
assertHasScope s_exp = asserts $ \g -> do
    let s_act = scopes g
    let msg   = printf "Expected scope %s to be part of graph, but got %s." (show s_exp) (show s_act)
    assertBool msg (s_exp <= s_act)

assertNotHasScope :: Sc -> SGTest l d ()
assertNotHasScope s_exp = asserts $ \g -> do
    let s_act = scopes g
    let msg   = printf "Expected scope %s not to be part of graph, but actually has up to %s." (show s_exp) (show s_act)
    assertBool msg (s_exp > s_act)

-- edges

assertHasEdge :: Sc -> Label -> Sc -> SGTest Label Data ()
assertHasEdge src lbl tgt = asserts $ \g -> do
    let e_act = entries g src
    let msg   = printf "Expected edge %s-%s->%s to be part of graph, but got %s." (show src) (show lbl) (show tgt) (show e_act)
    assertBool msg $ (lbl, Left tgt) `elem` e_act


assertScopeHasNoEdges :: (Eq l, Eq d, Show l, Show d) => Sc -> SGTest l d ()
assertScopeHasNoEdges s = asserts $ \g -> do
    let e_act = edgesOf g s
    let msg   = printf "Expected no edges for %s in the graph, but got %s." (show s) (show e_act)
    assertEqual msg [] e_act


assertHasNoEdges :: (Eq l, Eq d, Show l, Show d) => SGTest l d ()
assertHasNoEdges = iterateScopesIO assertScopeHasNoEdges


-- sinks

assertHasSink :: (Eq l, Eq d, Show l, Show d) => Sc -> l -> d -> SGTest l d ()
assertHasSink src lbl d = asserts $ \g -> do
    let e_act = entries g src
    let msg   = printf "Expected edge %s-%s->%s to be part of graph, but got %s." (show src) (show lbl) (show d) (show e_act)
    assertBool msg $ (lbl, Right d) `elem` e_act


assertScopeHasNoSinks :: (Eq l, Eq d, Show l, Show d) => Sc -> SGTest l d ()
assertScopeHasNoSinks s = asserts $ \g -> do
    let e_act = sinksOf g s
    let msg   = printf "Expected no edges for %s in the graph, but got %s." (show s) (show e_act)
    assertEqual msg [] e_act


assertHasNoSinks :: SGTest Label Data ()
assertHasNoSinks = iterateScopesIO assertScopeHasNoSinks

-- closed edge

assertClosed :: (Eq l, Eq d, Show l, Show d) => Sc -> l -> SGTest l d ()
assertClosed s lbl = asserts $ \g -> do
    let c_act = clos g s
    let msg   = printf "Expected edge %s-%s->? to be closed in graph, but got %s." (show s) (show lbl) (show c_act)
    assertBool msg $ lbl `elem` c_act


assertScopeHasNoClosedEdges :: (Eq l, Eq d, Show l, Show d) => Sc -> SGTest l d ()
assertScopeHasNoClosedEdges s = asserts $ \g -> do
    let c_act = clos g s
    let msg   = printf "Expected no closed edges for %s in the graph, but got %s." (show s) (show c_act)
    assertEqual msg [] c_act


assertHasNoClosedEdges :: (Eq l, Eq d, Show l, Show d) => SGTest l d ()
assertHasNoClosedEdges = iterateScopesIO assertScopeHasNoClosedEdges
