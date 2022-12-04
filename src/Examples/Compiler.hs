module Example.Compiler where

import Hefty
import Elab
import Free


data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | LetS String Expr Expr


denote :: Expr -> (String -> r Int) -> LVar r (r Int)
denote (Num i) _ = lit i
denote (Add e1 e2) nv = do
  v1 <- denote e1 nv
  v2 <- denote e2 nv
  plus v1 v2
denote (Sub e1 e2) nv = do
  v1 <- denote e1 nv
  v2 <- denote e2 nv
  minus v1 v2
denote (LetS x e body) nv = do
  v <- denote e nv
  llet v (\ v -> denote body (\ y -> if y == x then v else (nv y)))


data Lit r k
  = Lit Int (r Int -> k)
  deriving Functor

data Arith r k
  = Plus  (r Int) (r Int) (r Int -> k)
  | Minus (r Int) (r Int) (r Int -> k)
  deriving Functor

data Let r h k
  = forall x y. Let (r x) (r x -> h (r y)) (r y -> k)

deriving instance (Functor (Let r h))

instance HFunctor (Let r) where
  hmap f (Let rx m k) = Let rx (f . m) k

lit :: ( HFunctor h
       , Lift (Lit r) <| h )
    => Int -> Hefty h (r Int)
lit i = lift $ Lit i

plus :: ( HFunctor h
        , Lift (Arith r) <| h )
     => r Int -> r Int -> Hefty h (r Int)
plus r1 r2 = lift $ Plus r1 r2

minus :: ( HFunctor h
         , Lift (Arith r) <| h )
      => r Int -> r Int -> Hefty h (r Int)
minus r1 r2 = lift $ Minus r1 r2

llet :: forall r x y h.
        Let r <| h
     => r x -> (r x -> Hefty h (r y)) -> Hefty h (r y)
llet x body = Op $ injH $ Let x body Return



-- X86

data Movq r k
  = forall x. Movq (r x) (r x -> k)

deriving instance Functor (Movq r)

data Arithq r k
  = Addq (r Int) (r Int) k
  | Subq (r Int) (r Int) k
  deriving Functor

movq :: Movq r < f => r x -> Free f (r x)
movq r = Do $ inj $ Movq r Pure

addq :: Arithq r < f => r Int -> r Int -> Free f ()
addq r1 r2 = Do $ inj $ Addq r1 r2 $ Pure ()

subq :: Arithq r < f => r Int -> r Int -> Free f ()
subq r1 r2 = Do $ inj $ Subq r1 r2 $ Pure ()

eLet :: ( Functor f
        , Movq r < f )
     => Elab (Let r) f
eLet = Alg $ \ x -> case x of
  Let x m k -> do
    r <- movq x
    m r >>= k

eArith :: ( Functor f
          , Movq r < f
          , Arithq r < f )
       => Elab (Lift (Arith r)) f
eArith = Alg $ \ x -> case x of
  Lift (Plus r1 r2 k) -> do
    r1' <- movq r1
    addq r2 r1'
    k r1'
  Lift (Minus r1 r2 k) -> do
    r1' <- movq r1
    subq r2 r1'
    k r1'

data X86X :: * -> * where
  Imm :: Int    -> X86X Int
  Var :: String -> X86X x

pX86X :: X86X x -> String
pX86X (Imm i) = "$" ++ show i
pX86X (Var x) = x

pMovq :: FreeAlg (Movq X86X) (Int -> String)
pMovq = FreeAlg $ \ f i -> case f of
  (Movq rx k) ->
    let ry = Var ("x" ++ show i)
    in "    movq " ++ pX86X rx ++ ", " ++ pX86X ry ++ "\n" ++ k ry (i + 1)

pArithq :: FreeAlg (Arithq X86X) (Int -> String)
pArithq = FreeAlg $ \ f i -> case f of
  (Addq r1 r2 k) ->
    "    addq " ++ pX86X r1 ++ ", " ++ pX86X r2 ++ "\n" ++ k i
  (Subq r1 r2 k) ->
    "    subq " ++ pX86X r1 ++ ", " ++ pX86X r2 ++ "\n" ++ k i

pLit :: FreeAlg (Lit X86X) (Int -> String)
pLit = FreeAlg $ \ f -> case f of
  (Lit n k) -> k (Imm n)


pNop :: FreeAlg Nop (Int -> String)
pNop = FreeAlg $ \ f -> case f of


-- Compiler

type LVar r = Hefty (Lift (Lit r) ⊕ Lift (Arith r) ⊕ (Let r) ⊕ Lift Nop)

eCompile :: Hefty (Lift (Lit r) ⊕ Lift (Arith r) ⊕ (Let r) ⊕ Lift Nop) a
         -> Free (Lit r + Arithq r + Movq r + Nop) a
eCompile = hfold Pure (eLift ⊕ eArith ⊕ eLet ⊕ eLift)

pX86 :: Free (Lit X86X + Arithq X86X + Movq X86X + Nop) a
     -> String
pX86 f = fold (\ _ _ -> "") (freeAlg (pLit +~ pArithq +~ pMovq +~ pNop)) f 0


-- Example

example :: LVar X86X (X86X Int)
example = do
  n1 <- lit 1
  n2 <- lit 2
  plus n1 n2

example0 :: LVar X86X (X86X Int)
example0 = do
  n1 <- lit 1
  n2 <- lit 2
  n <- plus n1 n2
  llet n $ \ x -> do
    n5 <- lit 5
    llet n5 $ \ y -> do
       plus x y
