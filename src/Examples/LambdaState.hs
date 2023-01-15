module Examples.LambdaState where

import Hefty
import Free
import Elab
import Free.State
import Hefty.Lambda

example :: forall fun c.
           Hefty (Lambda c fun ⊕ Lift (State Int) ⊕ Lift Nop) Int
example = do
  liftH0 (Put (1 :: Int))
  (f :: fun (c Int) Int) <- lambda (\ (x :: c Int) -> do
                                       n1 <- var @fun x
                                       n2 <- var @fun x
                                       return $ n1 + n2)
  apply f incr
  where
    incr = do (n :: Int) <- liftH Get; liftH0 (Put $ n + 1); liftH Get

testExampleCBV :: Int
testExampleCBV =
  snd $ un $ flip (handle_ hState) 0
      $ hfold @(Free (State Int + Nop)) return (eLambdaCBV ⊕ eLift ⊕ eLift)
      $ example
-- = 4

testExampleCBN :: Int
testExampleCBN =
  snd $ un $ flip (handle_ hState) 0
      $ hfold @(Free (State Int + Nop)) return (eLambdaCBN ⊕ eLift ⊕ eLift)
      $ example
-- = 5

testExampleCBN' :: Int
testExampleCBN' =
  snd $ snd $ un
      $ flip (handle_ hState) []
      $ flip (handle_ hState) 0
      $ hfold @(Free (State Int + State [Pack] + Nop)) return (eLambdaCBN' ⊕ eLift ⊕ eLift)
      $ example
-- = 4
