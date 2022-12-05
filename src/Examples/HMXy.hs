module Examples.HMXy where

import Free

import Data.Term
import Data.List

import Free.Error
import Free.Logic.Exists
import Free.Logic.Equals

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

-- Type context
type Ctx = [(String, Ty)]

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

-- Type checking
tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Error String < f )
   => MLy -> Ctx -> Ty -> Free f ()
tc (Num _) _ t = conv t numT
tc (Plus e1 e2) ctx t = do
  conv t numT
  tc e1 ctx numT
  tc e2 ctx numT
tc (Abs x b) ctx t = do
  s <- exists
  t' <- exists
  tc b ((x,s):ctx) t'
  conv t (funT s t')
tc (Ident x) ctx t = case lookup x ctx of
  Just t' -> equals t t'
  Nothing -> err "bad lookup"
tc (App f a) ctx t = do
  fun <- exists
  tc f ctx fun
  s <- exists
  conv fun (funT s t)
  tc a ctx s
tc (Let x e body) ctx t = do
  s <- exists
  tc e ctx s
  st <- inspect s
  let s_fvs = fv st
  let ctx_fvs = concat $ map (fv . snd) $ ctx
  let gens = s_fvs \\ ctx_fvs
  tc body ((x,schemeT gens st):ctx) t

-- Running the type checker
runTC :: MLy -> Either String Ty
runTC e =
  let x = un
        $ handle hErr
        $ flip (handle_ hExists) 1
        $ flip (handle_ hEquals) Map.empty
        ( tc e [] (Var 0)
          :: Free (Equals Ty + Exists Ty + Error String + Nop) () )
  in case x of
    Left s -> Left s
    Right (Left (UnificationError t1 t2)) -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right (_, u)) -> Right $ projTermVar u 0


{-

typecheck (Let x e1 e2) = do
  T ← freeze (typecheck e1)
  bind x to T in (typecheck e2)

-}

