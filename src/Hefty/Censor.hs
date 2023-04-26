module Hefty.Censor where

import Free
import Free.Out
import Hefty

data Censor m k
  = forall a. Censor (String -> String) (m a) (a -> k)

deriving instance forall m. Functor (Censor m)

censorH :: Censor <| h => (String -> String) -> Hefty h a -> Hefty h a
censorH f m = Op $ injH $ Censor f m Return

eCensor :: ( Out < f
           , Functor f )
        => Alg Censor (Free f)
eCensor = Alg { alg = \ (Censor f m k) -> do
                  (s, x) <- hup (handle hOut) m
                  out (f s)
                  k x }

instance HFunctor Censor where
  hmap f (Censor g m k) = Censor g (f m) k 
