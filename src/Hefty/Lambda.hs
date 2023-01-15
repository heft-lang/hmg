module Hefty.Lambda where

import Hefty
import Free
import Elab
import Unsafe.Coerce

import Free.State

data Lambda c fun f k
  = forall t1 t2. Lambda (c t1 -> f t2)        (fun (c t1) t2 -> k)
  | forall t.     Var   (c t)                  (t             -> k)
  | forall t1 t2. Apply (fun (c t1) t2) (f t1) (t2            -> k)

deriving instance forall c fun f. Functor (Lambda c fun f)

instance HFunctor (Lambda c fun) where
  hmap f (Lambda body   k) = Lambda (f . body) k
  hmap _ (Var x         k) = Var x k
  hmap f (Apply fun arg k) = Apply fun (f arg) k

lambda :: forall fun c h t1 t2.
          Lambda c fun <| h
       => (c t1 -> Hefty h t2)
       -> Hefty h (fun (c t1) t2)
lambda body = Op $ injH $ Lambda body Return

var :: forall fun c h t.
       Lambda c fun <| h
    => c t -> Hefty h t
var x = Op $ injH @(Lambda c fun) $ Var x Return

apply :: forall fun c h t1 t2.
         Lambda c fun <| h
      => (fun (c t1) t2) -> Hefty h t1 -> Hefty h t2
apply fun arg = Op $ injH $ Apply fun arg Return


-- call-by-value interpretation

newtype Fun f t1 t2 = Fun { app :: t1 -> Free f t2 }

eLambdaCBV :: forall f.
              Functor f
           => Elab (Lambda Id (Fun f)) f
eLambdaCBV = Alg $ \ x -> case x of
  Lambda body   k -> k (Fun body)
  Var x         k -> k (unId x)
  Apply fun arg k -> do
    v <- arg
    (app fun $ Id v) >>= k
           

-- call-by-name interpretation

eLambdaCBN :: forall f.
              Functor f
           => Elab (Lambda (Free f) (Fun f)) f
eLambdaCBN = Alg $ \ x -> case x of
  Lambda body   k -> k (Fun body)
  Var x         k -> x >>= k
  Apply fun arg k -> (app fun arg) >>= k


-- call-by-need interpretation

newtype Thunk f a = Thunk { force :: (Int, Free f a) }

data Pack = forall v. Pack (Maybe v)

update :: Thunk f v -> v -> [Pack] -> [Pack]
update _ _ [] = undefined
update (Thunk (0, _)) v (_:p) = Pack (Just v) : p
update (Thunk (i, m)) v (t:p) | i > 0 = t:update (Thunk (i-1,m)) v p
                              | otherwise = undefined

insert :: Free f v -> [Pack] -> (Thunk f v, [Pack])
insert m p = (Thunk (length p, m), p ++ [Pack Nothing])

eLambdaCBN' :: forall f.
               ( Functor f
               , State [Pack] < f )
            => Elab (Lambda (Thunk f) (Fun f)) f
eLambdaCBN' = Alg $ \ x -> case x of
  Lambda body   k -> k (Fun body)
  Var x         k -> do
    (st :: [Pack]) <- get
    let (i, m) = force x
    case st !! i of
      Pack Nothing  -> do
        v <- m
        let v' = unsafeCoerce v
        (st' :: [Pack]) <- get
        put (update x v' st')
        k v'
      Pack (Just v) -> k (unsafeCoerce v)
  Apply fun arg k -> do
    st <- get
    let (t, st') = insert arg st
    put st'
    (app fun t) >>= k

