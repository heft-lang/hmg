module Examples.HMScope where

--import Debug.Trace

import Free

import Data.Regex
import Data.Term
import Data.List

import Free.Error
import Free.Logic.Exists
import Free.Logic.Equals
import Free.Logic.Generalize
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)

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

instance Show Ty where
  show (Const i) = "α" ++ show i
  show (Var i) = "α" ++ show i
  show (Term "∀" ts) = "(∀ " ++ intercalate " " (map show (init ts)) ++ ". " ++ show (last ts) ++ ")"
  show (Term "->" [t1, t2]) = show t1 ++ " -> " ++ show t2
  show (Term "Num" []) = "Num"
  show (Term f ts) = "(" ++ f ++ intercalate " " (map show ts) ++ ")"

-- Type construction
numT = Term "Num" []
funT s t = Term "->" [s, t]
schemeT xs t | length xs > 0 = Term "∀" (map Const xs ++ [t])
             | otherwise = t

-- Free variables
fv :: Term Int -> [Int]
fv (Const _) = []
fv (Var i) = [i]
fv (Term f ts) | f /= "∀" = nub $ concat $ map fv ts
               | otherwise = let bs = concat $ map c2fv (init ts)
                 in nub (fv (last ts) \\ bs)
  where
    c2fv (Const i) = [i]
    c2fv _ = []

-- Labels & declarations
data Label = P | D deriving (Eq, Show)
data Decl = Decl String Ty deriving (Eq, Show)

projTy (Decl _ t) = t

-- scope graph lib configuration/convenience
wildcard = Atom P `Pipe` Atom D

edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

instL :: ( Functor f
          , Generalize [Int] Ty < f
          , Equals Ty < f
          , Error String < f )
       => Ty -> Ty -> Free f ()
instL t1 t2 = do
  t1' <- instantiate @[Int] t1
  equals t1' t2


-- Type checker for an MLy language
tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Generalize [Int] Ty < f
      , Error String < f
      , Scope Sc Label Decl < f )
   => MLy -> Sc -> Ty -> Free f ()
tc (Num _) _ t = equals t numT
tc (Plus e1 e2) sc t = do
  equals t numT
  t1 <- exists
  tc e1 sc t1
  instL t1 numT

  t2 <- exists
  tc e2 sc t2
  instL t2 numT
tc (Abs x b) sc t = do
  s <- exists
  t' <- exists
  sc' <- new
  edge sc' P sc
  sink sc' D (Decl x s)
  tc b sc' t'
  instL t (funT s t')
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
  instL fun (funT s t)
  tc a sc s
tc (Let x e body) sc t = do
  s <- exists
  tc e sc s
  st <- inspect s
  ds <- query
          sc
          ((Star wildcard) `Dot` Atom D)
          (\ p1 p2 -> lenPath p1 < lenPath p2)
          (\ (_ :: Decl) -> True)
  let ctx_fvs = concat $ map (\ (Decl _ t) -> fv t) ds
  let gens = fv st \\ ctx_fvs
  sc' <- new
  edge sc' P sc
  sink sc' D (Decl x (schemeT gens st))
  tc body sc' t


-- Running the type checker
runTC :: MLy -> Either String Ty
runTC e =
  let x = un
        $ handle hErr
        $ flip (handle_ hScope) emptyGraph
        $ flip (handle_ hEquals) Map.empty
        $ flip (handle_ hExists) 0
        $ handle (hGeneralize
                    fv
                    schemeT
                    (\ t -> do
                       t <- inspect t
                       case t of
                         Term "∀" ts -> let gens = init ts; t' = last ts in do
                           substs <- mapM
                                       (\ x -> case x of
                                           Const i -> do y <- exists; return (i,y)
                                           _       -> err "Bad quantifier")
                                       gens
                           return $ substsIn substs t'
                         _ -> return t))
        (do t <- exists
            tc e 0 t
        :: Free ( Generalize [Int] Ty
                + Exists Ty
                + Equals Ty
                + Scope Sc Label Decl
                + Error String
                + Nop )
                () )
  in case x of
    Left s                                   -> Left s
    Right (Left (UnificationError t1 t2), _) -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right (_, u), _)                  -> Right $ inspectVar u 0


{-

> runTC (Let "g" (Abs "y" (Let "f" (Abs "x" $ Ident "y") (Let "_" (App (Ident "f") (Num 0)) (Ident "f")))) (Ident "g"))
Right (Term "\8704" [Const 7,Term "->" [Var 7,Term "\8704" [Const 5,Term "->" [Var 5,Var 7]]]])

-}
