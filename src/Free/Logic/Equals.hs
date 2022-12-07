module Free.Logic.Equals where

-- import Debug.Trace

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

-- Unification yields a unification map or errs
-- TODO: no occurs check
unify :: (Eq c, Show c) => Term c -> Term c -> UMap c -> Either (UErr (Term c)) (UMap c)
unify (Var i) c u | c /= Var i = if Map.member i u
                                 then unify (u Map.! i) c (Map.update (const Nothing) i u)
                                 else Right
                                    $ Map.alter (const $ Just c) i
                                    $ Map.map (substIn i c) u
                  | otherwise = Right $ u
unify c (Var i) u = unify (Var i) c u
unify (Const c1) t2 u | t2 == Const c1 = Right u
                      | otherwise = Left $ UnificationError (Const c1) t2
unify t1 (Const c2) u | t1 == Const c2 = Right u
                      | otherwise = Left $ UnificationError t1 (Const c2)
unify (Term s1 as1) (Term s2 as2) u | s1 /= s2 || length as1 /= length as2 =
                                      Left $ UnificationError (Term s1 as1) (Term s2 as2)
                                    | otherwise = foldr
                                      (\ (t1, t2) ru -> do
                                          u' <- ru
                                          unify t1 t2 u')
                                      (Right u)
                                      (zip as1 as2)


-- -- Project a term without any free variables; or fail
-- -- TODO: no occurs check
-- projTerm :: UMap c -> Int -> Maybe (Term c)
-- projTerm u i = do
--   Map.lookup i u >>= foldTerm
--     (return . Const)
--     (projTerm u)
--     (\ s' tsm -> do
--         ts' <- mapM id tsm
--         return $ Term s' ts')

-- Project a term that may contain free variables
-- TODO: no occurs check
inspectVar :: UMap c -> Int -> Term c
inspectVar u i = case Map.lookup i u of
  Just t -> foldTerm
    Const
    (inspectVar u)
    Term
    t
  Nothing -> Var i


hEquals :: ( Eq c
           , Show c
           , Functor f' )
        => Handler_ (Equals (Term c)) a (UMap c) f' (Either (UErr (Term c)) (a, UMap c))
hEquals = Handler_ {
    ret_ = \ x m -> return $ Right (x, m)
  , hdl_ = \ f m -> case f of
      Equals t1 t2 k ->
        case unify t1 t2 m of
          Right m' -> k m'
          Left err -> return $ Left err
      Inspect t k ->
        k (foldTerm Const (inspectVar m) Term t) m
  }

