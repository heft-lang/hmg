module Free.Reader where

import Free
import Hefty
import Elab

data Reader r k = Reader (r -> k)
  deriving Functor

reader :: Reader r < f => Free f r
reader = Do $ inj $ Reader Pure

readerH :: Lift (Reader r) <| h => Hefty h r
readerH = liftH $ Reader


---------------
--- HANDLER ---
---------------

hReader :: Functor f
        => Handler_ (Reader r) a r f a
hReader = Handler_ {
    ret_ = \ x _ -> return x
  , hdl_ = \ f r -> case f of
      Reader k -> k r r
  }
