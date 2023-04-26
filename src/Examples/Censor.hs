module Examples.Censor where

import Debug.Trace

import Free
import Free.Out
import Free.In
import Hefty.Censor
import Hefty
import Elab


helloName :: Free (Out + In + Nop) ()
helloName = do
  out "What is your Name?"
  name <- input
  out ("Hello, " ++ name)


runHelloName :: [String] -> (String, ())
runHelloName ins = un
  $ flip (handle_ hIn) ins
  $ handle hOut
  $ helloName


censorHello :: Hefty (Lift Out ⊕ Censor ⊕ Lift Nop) ()
censorHello =
  censorH f $ do
    outH "Hello, "
    outH "world!"
  where
    f s = if (s == "Hello, ") then "Goodbye, " else s


runCensorHello :: (String, ())
runCensorHello = un
  $ handle hOut
  $ ( elaborate (eLift ⊚ eCensor ⊚ eLift) censorHello
    :: Free (Out + Nop) () )


hOut' :: Functor f'
      => (String -> String)
      -> Handler Out a f' (String, a)
hOut' f = Handler { ret = \ x -> return ("", x)
                  , hdl = \ (Out s k) -> do
                      (s', x) <- k
                      return ((f s) ++ s', x) }


eCensor' :: ( Out < f
            , Functor f )
         => Alg Censor (Free f)
eCensor' = Alg { alg = \ (Censor f m k) -> do
                  (s, x) <- hup (handle (hOut' f)) m
                  out (f s)
                  k x }

runCensorHello' :: (String, ())
runCensorHello' = un
  $ handle hOut
  $ ( elaborate (eLift ⊚ eCensor' ⊚ eLift) censorHello
    :: Free (Out + Nop) () )
