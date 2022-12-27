module Hefty.PEG where

import Free
import Hefty
import Elab

import Free.Reader
import Free.Logic.Or

data Eq s => PEG s d h k
  = Match (s -> Maybe (d, s)) (d -> k)
  | Group (h d) (d -> k)
  | Optional (h d) (Maybe d -> k)
  | Star (h d) ([d] -> k)
  | Plus (h d) ((d, [d]) -> k)
  | AndP (h d) k
  | NotP (h d) k
  | ChoiceP (h d) (h d) (d -> k)

ePEG :: forall t d f.
        ( Eq t
        , Functor f
        , Bot < f
        , Or < f
        , Reader [t] < f )
     => Elab (PEG [t] d) f
ePEG = Alg $ \case
  Match g k -> do
    (ts :: [t]) <- reader
    case g ts of
      Nothing -> bot
      Just (d, ts') -> fmap unId $
        hup ( fmap (fmap Id)
            $ flip (handle_ hReader) ts' ) $
        k d
  Group m k -> _
