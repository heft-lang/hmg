module Hefty.Local where

import Hefty
import Elab

import Free
import Free.Reader

data Local r f k
  = forall a. Local (r -> r) (f a) (a -> k)

deriving instance forall r f. Functor (Local r f)

instance HFunctor (Local r) where
  hmap f (Local g m k) = Local g (f m) k


-------------------
--- ELABORATION ---
-------------------

eLocal :: forall r f.
          ( Functor f
          , Reader r < f )
       => Elab (Local r) f
eLocal = Alg $ \ x -> case x of
  Local g m k -> do
    (r :: r) <- reader
    v <- hup (flip (handle_ hReader) (g r) . fmap Id) m
    k (unId v)


