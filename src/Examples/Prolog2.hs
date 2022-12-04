module Example.Prolog2 where

import Hefty
import Free
import Elab

import Data.Term

import Val.Subtype

import Hefty.Lambda

import Free.NonDet
import Free.Logic.Exists
import Free.Logic.Equals

import qualified Data.Map as Map


-----------
--- RUN ---
-----------

instance String ⊂ Term c where
  injV s = Term s []

-- A run function that computes all possible results (not just the first one),
-- computed in a depth-first search manner
runProlog h =
  un $
  handle hNonDet $
  flip (handle_ hEquals) (Map.empty) $
  flip (handle_ hExists) 0 $
  ( elaborate ( eLift @NonDet
              ⊕ eLift @(Equals (Term Int))
              ⊕ eLift @(Exists (Term Int))
              ⊕ eLambdaCBV
              ⊕ eLift )
    h
  :: Free (Exists (Term Int) + Equals (Term Int) + NonDet + Nop) () )



{-
B(X) :- X == "foo"

A(X, Y) :- X == Y, B(X)

:- ∃ R S. A(R, S)
-}
prog0 :: forall trm c fun.
         (String ⊂ trm)
      => Hefty (Lift NonDet ⊕ Lift (Equals trm) ⊕ Lift (Exists trm) ⊕ Lambda c fun ⊕ Lift Nop) ()
prog0 = do
  (b :: fun (c trm) ()) <- lambda (\ x -> do
                                      x <- var @fun x
                                      equalsH x (injV "foo"))
  (a :: fun (c (trm, trm)) ()) <- lambda (\ xy -> do
                                             (x, y) <- var @fun xy
                                             equalsH x y
                                             apply b (return x))
  r <- existsH
  s <- existsH
  apply a (return (r, s))

-- λ> runProlog prog0
-- [Right ((),fromList [(0,Var 1),(1,Term "foo" [])])]


-- another test involving disjunction:

{-
B(X) :- X == "foo"
      ; X == "bar".

A(X, Y) :- X == Y, B(X).

:- ∃ R S. A(R, S)
-}

prog1 :: forall trm c fun.
         (String ⊂ trm)
      => Hefty (Lift NonDet ⊕ Lift (Equals trm) ⊕ Lift (Exists trm) ⊕ Lambda c fun ⊕ Lift Nop) ()
prog1 = do
  (b :: fun (c trm) ()) <- lambda (\ x -> do
                                      x <- var @fun x
                                      equalsH x (injV "foo")
                                        `orH`
                                        equalsH x (injV "bar"))
  (a :: fun (c (trm, trm)) ()) <- lambda (\ xy -> do
                                             (x, y) <- var @fun xy
                                             equalsH x y
                                             apply b (return x))
  r <- existsH
  s <- existsH
  apply a (return (r, s))


-- λ> runProlog prog1
-- [ Right ((),fromList [(0,Var 1),(1,Term "foo" [])])
-- , Right ((),fromList [(0,Var 1),(1,Term "bar" [])]) ]
