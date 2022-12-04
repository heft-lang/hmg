module Free.Logic.Equals where

import Free
import Hefty
import Elab

import Data.Term
import qualified Data.Map as Map

data Equals trm k
  = Equals trm trm k
  | Inspect trm (trm -> k)
  deriving Functor

equals :: forall trm f.
          Equals trm < f
       => trm -> trm -> Free f ()
equals t1 t2 = Do $ inj $ Equals t1 t2 (Pure ())

equalsH :: forall trm h.
           Lift (Equals trm) <| h
        => trm -> trm -> Hefty h ()
equalsH t1 t2 = liftH0 $ Equals t1 t2

inspect :: forall trm f.
           Equals trm < f
        => trm -> Free f trm
inspect t = Do $ inj $ Inspect t Pure

inspectH :: forall trm h.
           Lift (Equals trm) <| h
        => trm -> Hefty h trm
inspectH t = liftH (Inspect t)


---------------
--- HANDLER ---
---------------

-- A handler that uses unification to resolve equality constraints

-- Unification errors
-- TODO: support error info
data UErr trm
  = UnificationError trm trm
  deriving Show

-- A unification map maps variables to terms
type UMap c = Map.Map Int (Term c)

-- Project a term without any free variables; or fail
-- TODO: no occurs check
projTerm :: UMap c -> Int -> Maybe (Term c)
projTerm u i = do
  Map.lookup i u >>= foldTerm
    (return . Const)
    (projTerm u)
    (\ s' tsm -> do
        ts' <- mapM id tsm
        return $ Term s' ts')

-- Project a term that may contain free variables
-- TODO: no occurs check
projTermVar :: UMap c -> Int -> Term c
projTermVar u i = case Map.lookup i u of
  Just t -> foldTerm
    Const
    (projTermVar u)
    Term
    t
  Nothing -> Var i

-- Merge overlapping variables by unifying them
-- TODO: no occurs check
merge :: Eq c => UMap c -> UMap c -> Either (UErr (Term c)) (UMap c)
merge m1 m2 = do
  let i = Map.intersectionWith (,) m1 m2
  m <- Map.foldrWithKey
         (\ k (t1, t2) c0 -> do
             m' <- unify t1 t2
             m0' <- c0
             return $ m' `Map.union` ((Map.singleton k t1) `Map.union` m0'))
         (return Map.empty)
         i
  return $ m `Map.union` (m1 `Map.union` m2)

-- Shorthand
merges :: Eq c => [UMap c] -> Either (UErr (Term c)) (UMap c)
merges = foldr (\ m c -> do m' <- c; merge m m') (return Map.empty)

-- Unification yields a unification map or errs
-- TODO: no occurs check
unify :: Eq c => Term c -> Term c -> Either (UErr (Term c)) (UMap c)
unify (Var i) c | c /= Var i = Right $ Map.fromList [(i, c)]
                | otherwise = Right $ Map.empty
unify c (Var i) = unify (Var i) c
unify (Const c1) t2 | t2 == Const c1 = Right Map.empty
                    | otherwise = Left $ UnificationError (Const c1) t2
unify t1 (Const c2) | t1 == Const c2 = Right Map.empty
                    | otherwise = Left $ UnificationError t1 (Const c2)
unify (Term s1 as1) (Term s2 as2) | s1 /= s2 || length as1 /= length as2 =
                                    Left $ UnificationError (Term s1 as1) (Term s2 as2)
                                  | otherwise = do
  m1s <- mapM (uncurry unify) (zip as1 as2)
  merges m1s


hEquals :: ( Eq c
           , Functor f' )
        => Handler_ (Equals (Term c)) a (UMap c) f' (Either (UErr (Term c)) (a, UMap c))
hEquals = Handler_ {
    ret_ = \ x m -> return $ Right (x, m)
  , hdl_ = \ f m -> case f of
      Equals t1 t2 k ->
        case unify t1 t2 of
          Right m' -> case merge m m' of
            Right m'' -> k m''
            Left err -> return $ Left err
          Left err -> return $ Left err
      Inspect t k ->
        k (foldTerm Const (projTermVar m) Term t) m
  }

