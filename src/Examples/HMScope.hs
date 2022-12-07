module Examples.HMScope where

import Debug.Trace

import Free

import Data.Regex
import Data.Term
import Data.List

import Free.Error
import Free.Logic.Exists
import Free.Logic.Equals
import Free.Scope

import qualified Data.Map as Map

-- Language to type check
data MLy
  = Num Int
  | Plus MLy MLy
  | Abs String MLy
  | Ident String
  | App MLy MLy
  | Let String MLy MLy

-- Types
type Ty = Term Int

-- Type constructors
numT = Term "Num" []
funT s t = Term "->" [s, t]
schemeT xs t | length xs > 0 = Term "∀" (map Const xs ++ [t])
             | otherwise = t

data Label = P | D deriving (Eq, Show)
data Decl = Decl String Ty deriving (Eq, Show)

wildcard = Atom P `Pipe` Atom D

projTy (Decl _ t) = t

edge' :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge' = edge @_ @Label @Decl

new' :: Scope Sc Label Decl < f => Free f Sc
new' = new @_ @Label @Decl

sink' :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink' = sink @_ @Label @Decl

conv :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Error String < f )
     => Ty -> Ty -> Free f ()
conv t s = do
  t <- inspect t
  s <- inspect s
  conv' t s
  where
    conv' (Term "∀" ts) t =
      let gens = init ts
          t'   = last ts
      in do
        substs <- mapM
                    (\ x -> case x of
                        Const i -> do y <- exists; return (i,y)
                        _ -> err "Bad quantifier")
                   gens
        equals (substsIn substs t') t
    conv' t s@(Term "∀" _) = conv' s t
    conv' t s = equals t s

tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Error String < f
      , Scope Sc Label Decl < f )
   => MLy -> Sc -> Ty -> Free f ()
tc (Num _) _ t = equals t numT
tc (Plus e1 e2) sc t = do
  t1 <- exists
  t2 <- exists
  conv t numT
  tc e1 sc t1
  conv t1 numT
  tc e2 sc t2
  conv t2 numT
tc (Abs x b) sc t = do
  s <- exists
  t' <- exists
  sc' <- new'
  edge' sc' P sc
  sink' sc' D (Decl x s)
  tc b sc' t'
  conv t (funT s t')
tc (Ident x) sc t = do
  ds <- query
          sc
          (Star (Atom P) `Dot` Atom D)
          (\ p1 p2 -> lenPath p1 < lenPath p2)
          (\ (Decl y _) -> x == y)
  if length ds == 1
    then equals t (projTy (head ds))
    else if length ds == 0
         then err $ "Query failed: unbound identifier " ++ x
         else err $ "Query yielded ambiguous binders for " ++ x
tc (App f a) sc t = do
  fun <- exists
  tc f sc fun
  s <- exists
  conv fun (funT s t)
  tc a sc s
tc (Let x e body) sc t = do
  s <- exists
  tc e sc s
  st <- inspect s
  ds <- query
          sc
          (wildcard `Dot` Atom D)
          (\ p1 p2 -> lenPath p1 < lenPath p2)
          (\ (_ :: Decl) -> True)
  let s_fvs = fv st
  let ctx_fvs = concat $ map (\ (Decl _ t) -> fv t) ds
  let gens = s_fvs \\ ctx_fvs
  sc' <- new'
  edge' sc' P sc
  sink' sc' D (Decl x (schemeT gens st))
  tc body sc' t
  


-- Running the type checker
runTC :: MLy -> Either String Ty
runTC e =
  let x = un
        $ handle hErr
        $ flip (handle_ hScope) emptyGraph
        $ flip (handle_ hEquals) Map.empty
        $ flip (handle_ hExists) 0
        ( do t <- exists
             t' <- exists
             equals t t'
             tc e 0 t'
          :: Free (Exists Ty + Equals Ty + Scope Sc Label Decl + Error String + Nop) () )
  in case x of
    Left s -> Left s
    Right (Left (UnificationError t1 t2), _) -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right (_, u), _) -> Right $ inspectVar u 0
