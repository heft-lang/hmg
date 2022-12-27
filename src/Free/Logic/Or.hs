module Free.Logic.Or where

import Free
import Hefty
import Elab

data Or k
  = Or (Bool -> k)
  deriving Functor

data Bot k
  = Bot
  deriving Functor

orL :: Or < f => Free f a -> Free f a -> Free f a
orL k1 k2 = Do $ inj $ Or $ \case
  True -> k1
  False -> k2

orH :: ( Lift Or <| h
       , HFunctor h )
    => Hefty h a -> Hefty h a -> Hefty h a
orH k1 k2 = liftH $ \ k -> Or $ \case
  True  -> k1 >>= k
  False -> k2 >>= k

bot :: Bot < f => Free f a
bot = Do $ inj $ Bot

botH :: Lift Bot <| h => Hefty h a
botH = liftH $ const $ Bot


hChoice :: Functor f'
        => Handler Or a f' [a]
hChoice = Handler {
    ret = \ x -> return [x]
  , hdl = \case
      Or k -> do
        xs <- k True
        ys <- k False
        return (xs ++ ys)
  }

type LogicOr = Or + Bot

hOrBot :: Functor f'
       => Handler LogicOr a f' [a]
hOrBot = Handler {
    ret = \ x -> return [x]
  , hdl = \case
      L (Or k) -> do
        xs <- k True
        ys <- k False
        return (xs ++ ys)
      R Bot -> return []
  }
