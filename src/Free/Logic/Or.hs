module Free.Logic.Or where

import Free
import Hefty
import Elab

data Or k
  = Or (Bool -> k)
  | Bot
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

bot :: Or < f => Free f a
bot = Do $ inj $ Bot

botH :: Lift Or <| h => Hefty h a
botH = liftH $ const $ Bot

hOr :: Functor f'
    => Handler Or a f' [a]
hOr = Handler {
    ret = \ x -> return [x]
  , hdl = \case
      Or k -> do
        xs <- k True
        ys <- k False
        return (xs ++ ys)
      Bot -> return []
  }

hOrLeft :: Functor f'
        => Handler Or a f' (Maybe a)
hOrLeft = Handler {
    ret = return . Just
  , hdl = \case
      Or k -> do
        x <- k True
        case x of
          Just v -> return (Just v)
          Nothing -> k False
      Bot -> return Nothing
  }
