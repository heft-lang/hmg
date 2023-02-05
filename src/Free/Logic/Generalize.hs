module Free.Logic.Generalize where

import Free

import Data.List
import Data.Term


data Generalize vars trm k
  = Generalize vars trm (trm -> k)
  | Instantiate trm (trm -> k)
  deriving Functor

generalize :: forall vars trm f. Generalize vars trm < f => vars -> trm -> Free f trm
generalize xs t = Do $ inj $ Generalize xs t Pure

instantiate :: forall vars trm f. Generalize vars trm < f => trm -> Free f trm
instantiate t = Do $ inj $ Instantiate @vars t Pure


---------------
--- HANDLER ---
---------------

hGeneralize :: Functor f'
            => ([Int] -> Term c -> Term c)
            -> (Term c -> Free f' (Term c))
            -> Handler (Generalize [Int] (Term c)) a f' a
hGeneralize genT instT = Handler {
    ret = return
  , hdl = \ f -> case f of
      Generalize xs t k ->
        let fvs = fv t
            gens = fvs \\ xs
        in k $ genT gens t
      Instantiate t k -> instT t >>= k
  }
