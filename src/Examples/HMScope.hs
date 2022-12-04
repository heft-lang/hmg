module Examples.HMScope where

import Free

import Data.Regex
import Data.Term
--import Data.List

import Free.Error
import Free.Logic.Exists
import Free.Logic.Equals
import Free.Scope

-- Language to type check
data MLy
  = Num Int
  | Plus MLy MLy
  | Abs String MLy
  | Ident String
  | App MLy MLy
  -- | Let String MLy MLy

-- Types
type Ty = Term Int

-- Type constructors
numT = Term "Num" []
funT s t = Term "->" [s, t]
schemeT xs t = Term "∀" (map Const xs ++ [t])

type Sc = Int
data Label = P | D
data Decl = Decl String Ty

edge' :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge' = edge @_ @Label @Decl

new' :: Scope Sc Label Decl < f => Free f Sc
new' = new @_ @Label @Decl

sink' :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink' = sink @_ @Label @Decl

tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Error String < f
      , Scope Sc Label Decl < f )
   => MLy -> Sc -> Ty -> Free f ()
tc (Num _) _ t = equals t numT
tc (Plus e1 e2) sc t = do
  equals t numT
  tc e1 sc numT
  tc e2 sc numT
tc (Abs x b) sc t = do
  s <- exists
  t' <- exists
  sc' <- new'
  edge' sc' P sc
  sink' sc' D (Decl x s)
  tc b sc' t'
  equals t (funT s t')
tc (Ident x) sc t = do
  (Decl _ s) <- query
                  sc
                  (Star (Atom P) `Dot` Atom D)
                  (\ (Decl y _) -> x == y)
  equals s t
tc (App f a) sc t = do
  fun <- exists
  tc f sc fun
  s <- exists
  equals fun (funT s t)
  tc a sc s
