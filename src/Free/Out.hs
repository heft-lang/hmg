module Free.Out where

import Free
import Hefty
import Elab

data Out k = Out String k
  deriving Functor

out :: Out < f => String -> Free f ()
out str = Do (inj $ Out str $ Pure ())

outH :: Lift Out <| h => String -> Hefty h ()
outH str = liftH0 (Out str)

hOut :: Functor f => Free (Out + f) a -> Free f (String, a)
hOut (Pure x) = return ("", x)
hOut (Do (L (Out o k))) = do
  (o', v) <- hOut k
  return (o++"\n"++o', v)
hOut (Do (R op)) = Do (fmap hOut op)
