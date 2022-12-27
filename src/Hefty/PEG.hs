module Hefty.PEG where

import Free
import Hefty
import Elab

import Free.Reader
import Free.Logic.Or

data Eq s => PEG s d h k
  = Match (s -> Maybe (d, s)) (d -> k)
  | Group (h d) (d -> k)
  | Star (h d) ([d] -> k)
  | forall a. NotP (h a) k
  | forall a. ChoiceP (h a) (h a) (a -> k)

matchP :: ( Eq s
          , PEG s d <| h )
       => (s -> Maybe (d, s))
       -> Hefty h d
matchP g = Op $ injH $ Match g Return

groupP :: forall s d h.
          ( Eq s
          , PEG s d <| h )
       => Hefty h d
       -> Hefty h d
groupP m = Op $ injH $ Group @s m Return

starP :: forall s d h.
         ( Eq s
         , PEG s d <| h )
      => Hefty h d
      -> Hefty h [d]
starP m = Op $ injH $ Star @s m Return

notP :: forall s d h a.
        ( Eq s
        , PEG s d <| h )
     => Hefty h a
     -> Hefty h ()
notP m = Op $ injH $ NotP @s @d m (Return ())

choiceP :: forall s d h a.
           ( Eq s
           , PEG s d <| h )
        => Hefty h a
        -> Hefty h a
        -> Hefty h a
choiceP m1 m2 = Op $ injH $ ChoiceP @s @d m1 m2 Return

-- sugar

optP :: forall s d h a.
        ( Eq s
        , PEG s d <| h )
     => Hefty h d
     -> Hefty h d
optP m = choiceP @s @d m (matchP @s @d (const Nothing))

plusP :: forall s d h a.
         ( Eq s
         , PEG s d <| h )
      => Hefty h d
      -> Hefty h [d]
plusP m = do
  d <- m
  ds <- starP @s @d m
  return $ d:ds

andP :: forall s d h a.
        ( Eq s
        , PEG s d <| h )
     => Hefty h a
     -> Hefty h ()
andP m = notP @s @d (notP @s @d m)


ePEG :: forall t d f.
        ( Eq t
        , Functor f
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
  Group m k ->
    m >>= k
  -- Star m k -> do
  --   ds <- hup (handle hOr) m
  --   case ds of
  --     [] -> k []
  --     [d] -> 
