module Free.Error where

import Free
import Hefty
import Elab

data Error e k = Error e
  deriving Functor

err :: Error e < f => e -> Free f a
err e = Do $ inj $ Error e

errH :: Lift (Error e) <| h => e -> Hefty h a
errH e = liftH $ const $ Error e

hErr :: Functor f
     => Handler (Error e) a f (Either e a)
hErr = Handler
  (return . Right)
  (\ (Error e) -> return $ Left e)
