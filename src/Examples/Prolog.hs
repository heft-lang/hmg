module Examples.Prolog where

import Hefty
import Elab
import Free

import Free.NonDet

import qualified Data.Map as Map

data Clause c f k
  = forall a. Clause (a -> f ()) (c a -> k)
  | forall a. Call (c a) a k

deriving instance forall c f. Functor (Clause c f)

instance HFunctor (Clause c) where
  hmap f (Clause body k) = Clause (f . body) k
  hmap _ (Call c a k) = Call c a k

clause :: Clause c <| h
       => (a -> Hefty h ()) -> Hefty h (c a)
clause f = Op $ injH $ Clause f Return

call :: Clause c <| h
     => c a -> a -> Hefty h ()
call c a = Op $ injH $ Call c a (Return ())


data Vars trm k
  = Exists (trm -> k)
  | AssertEq trm trm k
  deriving Functor

exists :: Vars trm < f => Free f trm
exists = Do $ inj $ Exists Pure

existsH :: Lift (Vars trm) <| h => Hefty h trm
existsH = liftH Exists

assertEq :: forall trm f.
          Vars trm < f
       => trm -> trm -> Free f ()
assertEq t1 t2 = Do $ inj $ AssertEq t1 t2 (Pure ())

assertEqH :: forall trm h.
           Lift (Vars trm) <| h
        => trm -> trm -> Hefty h ()
assertEqH t1 t2 = liftH0 $ AssertEq t1 t2


{-

B(X) :- X == "foo"

A(X, Y) :- X == Y, B(X)

:- ∃ R S. A(R, S)

-}

class v ⊂ w where
  injV :: v -> w

prog0 :: forall trm c.
         (String ⊂ trm)
      => Hefty (Lift (Vars trm) ⊕ Clause c ⊕ Lift Nop) ()
prog0 = do
  b <- clause @c (\ (x :: trm) -> do
                    assertEqH x (injV "foo"))
  a <- clause @c (\ (x, y) -> do
                     assertEqH x y
                     call b x)
  r <- existsH
  s <- existsH
  call a (r,s)



-- handler of vars

data Term c = Const c
            | Var Int
            | Term String [Term c]

deriving instance Eq c => Eq (Term c)
deriving instance Show c => Show (Term c)

instance String ⊂ Term c where
  injV s = Term s []

type USto c = Map.Map Int (Term c)

data UErr
  = UnificationError
  deriving Show

-- Merge overlapping variables by unifying them

merge :: Eq c => USto c -> USto c -> Either UErr (USto c)
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

merges :: Eq c => [USto c] -> Either UErr (USto c)
merges = foldr (\ m c -> do m' <- c; merge m m') (return Map.empty)

unify :: Eq c => Term c -> Term c -> Either UErr (USto c)
unify (Var i) c | c /= Var i = Right $ Map.fromList [(i, c)]
                | otherwise = Right $ Map.empty
unify c (Var i) = unify (Var i) c
unify (Const c1) t2 | t2 == Const c1 = Right Map.empty
                    | otherwise = Left UnificationError
unify t1 (Const c2) | t1 == Const c2 = Right Map.empty
                    | otherwise = Left UnificationError
unify (Term s1 as1) (Term s2 as2) | s1 /= s2 || length as1 /= length as2 = Left UnificationError
                                  | otherwise = do
  m1s <- mapM (uncurry unify) (zip as1 as2)
  merges m1s

hVars :: ( Eq c
         , Functor f' )
      => Handler_ (Vars (Term c)) a (Int, USto c) f' (Either UErr (a, USto c))
hVars = Handler_ {
    ret_ = \ x (_, m) -> return $ Right (x, m)
  , hdl_ = \ f (i, m) -> case f of
      Exists k -> k (Var i) (i + 1, m)
      AssertEq t1 t2 k ->
        case unify t1 t2 of
          Right m' -> (case merge m m' of
            Right m'' -> k (i, m'')
            Left err -> return $ Left err)
          Left err -> return $ Left err
  }

-- a test:
progVar :: Free (Vars (Term Int) + Nop) ()
progVar = do
  (x :: Term Int) <- exists
  y               <- exists
  assertEq
    (Term "foo" [x, y, x])
    (Term "foo" [y, x, y])

-- λ> un $ handle_ hVars progVar (0, Map.empty)
-- Right ((),fromList [(0,Var 1),(1,Var 0)])


-- handler for clauses

newtype Ref f a = Ref { runRef :: a -> Free f () }

eClause :: Functor f => Elab (Clause (Ref f)) f
eClause = Alg $ \ h -> case h of
  Clause body k -> k $ Ref body
  Call ref a k -> do runRef ref a; k


-- un $ handle_ hVars (elaborate (eLift @(Vars (Term Int)) ⊕ eClause ⊕ eLift) prog0 :: Free (Vars (Term Int) + Nop) ()) (0, Map.empty)
-- Right ((),fromList [(0,Var 1),(1,Term "foo" [])])


-- another test involving disjunction:

{-

B(X) :- X == "foo"
      ; X == "bar".

A(X, Y) :- X == Y, B(X).

:- ∃ R S. A(R, S)

-}

prog1 :: forall trm c.
         (String ⊂ trm)
      => Hefty (Lift NonDet ⊕ Lift (Vars trm) ⊕ Clause c ⊕ Lift Nop) ()
prog1 = do
  b <- clause @c (\ (x :: trm) -> do
                    assertEqH x (injV "foo")
                    `orH`
                    assertEqH x (injV "bar"))
  a <- clause @c (\ (x, y) -> do
                     assertEqH x y
                     call b x)
  r <- existsH
  s <- existsH
  call a (r,s)

-- λ> un $ hNonDet $ handle_ hVars (elaborate (eLift @NonDet ⊕ eLift @(Vars (Term Int)) ⊕ eClause ⊕ eLift) prog1 :: Free (Vars (Term Int) + NonDet + Nop) ()) (0, Map.empty)
-- [Right ((),fromList [(0,Var 1),(1,Term "foo" [])]),Right ((),fromList [(0,Var 1),(1,Term "bar" [])])]
