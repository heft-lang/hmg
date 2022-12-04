module Free.Logic.Exists where

import Free
import Hefty
import Elab

import Data.Term

data Exists trm k
  = Exists (trm -> k)
  deriving Functor

exists :: Exists trm < f => Free f trm
exists = Do $ inj $ Exists Pure

existsH :: Lift (Exists trm) <| h => Hefty h trm
existsH = liftH Exists


---------------
--- HANDLER ---
---------------

-- A handler that creates fresh variables, assuming a suitably chosen initial
-- integer parameter

hExists :: Functor f'
        => Handler_ (Exists (Term c)) a Int f' a
hExists = Handler_ {
    ret_ = \ x _ -> return x
  , hdl_ = \ f i -> case f of
      Exists k -> k (Var i) (i + 1)
  }

