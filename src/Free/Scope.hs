module Free.Scope where

import Free
import Free.Error

import Data.Regex

data Path s l
  = Stop Sc
  | Step Sc l (Path s l)

data Scope s l d k
  = New (s -> k)
  | Edge s l s k
  | Sink s l d k
  | Query s (RE l) (Path s l -> Path s l -> Bool) (d -> Bool) ([d] -> k)

new :: forall s l d f.
       Scope s l d < f
    => Free f s
new = Do $ inj $ New @s @l @d $ Pure

edge :: forall s l d f.
        Scope s l d < f
     => s -> l -> s -> Free f ()
edge s l s' = Do $ inj $ Edge @s @l @d s l s' $ Pure ()

sink :: forall s l d f.
        Scope s l d < f
     => s -> l -> d -> Free f ()
sink s l d = Do $ inj $ Sink @s @l @d s l d $ Pure ()

query :: forall s l d f.
        Scope s l d < f
     => s -> RE l -> (Path s l -> Path s l -> Bool) -> (d -> Bool) -> Free f [d]
query s re po ad = Do $ inj $ Query s re po ad $ Pure


---------------
--- HANDLER ---
---------------

-- TODO: close when querying

type Sc = Int

data Graph l d = Graph { scopes :: Sc
                       , entries :: Sc -> Maybe [(l, Either Sc d)] }

addScope :: Graph l d -> (Sc, Graph l d)
addScope g = let sc = scopes g
  in (sc, Graph (sc + 1) (entries g))

addEdge :: ( Eq l
           , Show l )
        => Graph l d -> Sc -> l -> Sc -> Either String (Graph l d)
addEdge g s l s' =
  if s < scopes g
  then if s' < scopes g
       then case entries g s of
              Nothing -> Right $ rawAdd g s l s'
              Just edges ->
                case lookup l edges of
                  Nothing -> Right $ rawAdd g s l s'
                  Just _ ->
                    Left $ "Error: scope " ++ show s ++ " already has an edge labeled " ++ show l
       else Left $ "Invalid scope: " ++ show s'
  else Left $ "Invalid scope: " ++ show s
  where
    rawAdd g s l s' =
      Graph
        (scopes g)
        (\ sc -> if sc == s
          then maybe (Just [(l, Left s')]) (\ edges -> Just $ (l, Left s'):edges) (entries g s)
          else entries g sc)

addSink :: ( Eq l
           , Eq d
           , Show d
           , Show l )
        => Graph l d -> Sc -> l -> d -> Either String (Graph l d)
addSink g s l d =
  if s < scopes g
  then case sinksOf g s of
         [] -> Right $ rawAdd g s l d
         sinks ->
           if (l,d) `elem` sinks
           then Left $ "Error: there is already a declaration " ++ show d ++ " at label " ++ show l
           else Right $ rawAdd g s l d
  else Left $ "Invalid scope: " ++ show s
  where
    rawAdd g s l d =
      Graph
        (scopes g)
        (\ sc -> if sc == s
          then maybe (Just [(l, Right d)]) (\ edges -> Just $ (l, Right d):edges) (entries g s)
          else entries g sc)

sinksOf :: Graph l d -> Sc -> [(l, d)]
sinksOf g sc = concat
             $ maybe [] (map (\ (l, e) -> either
                               (const [])
                               (\ e -> [(l, e)])
                               e))
             $ entries g sc

edgesOf :: Graph l d -> Sc -> [(l, Sc)]
edgesOf g sc = concat
             $ maybe [] (map (\ (l, e) -> either
                               (\ sc' -> [(l, sc')])
                               (const [])
                               e))
             $ entries g sc

inPath :: Sc -> Path s l -> Bool
inPath sc (Stop sc') = sc == sc'
inPath sc (Step sc' _ p) = sc == sc' || inPath sc p

execQuery :: Eq l
          => Graph l d -> Sc -> RE l -> (Path s l -> Path s l -> Bool) -> (d -> Bool) -> [(d, l, Path s l)]
execQuery g sc re po ad = do
  let ps = findAll g sc re ad (Stop sc)
  shortest po ps
  where
    shortest :: (Path s l -> Path s l -> Bool) -> [(d, l, Path s l)] -> [(d, l, Path s l)]
    shortest _  [] = []
    shortest po (dp:dps) = go dp [] dps
      where
        go (d, l, p) a [] = (d, l, p):a
        go (d, l, p) a ((d',l',p'):dps) =
          if po p' p
          then if po p p'
               then go (d, l, p) ((d',l',p'):a) dps
               else go (d', l', p') [] dps
          else go (d, l, p) a dps
        
    
    findAll :: Eq l => Graph l d -> Sc -> RE l -> (d -> Bool) -> Path s l -> [(d, l, Path s l)]
    findAll g sc re ad p =
      if isEmpty re
      then map (\ (l, d) -> (d, l, p))
           $ filter (\ (_, d) -> ad d) $ sinksOf g sc
      else
        concat
        $ map
            (\ l ->
                ( concat
                $ map (\ (_, sc') ->
                         if inPath sc' p -- avoid cyclic paths
                         then []
                         else findAll g sc' re ad (Step sc' l p))
                $ filter (\ (l', _) -> l' == l) (edgesOf g sc) ))
            (frontier re)


hScope :: ( Eq l, Show l
          , Eq d, Show d
          , Functor f
          , Error String < f )
       => Handler_ (Scope Sc l d) a (Graph l d) f (a, Graph l d)
hScope = Handler_
  (\ x g -> return (x, g))
  (\ f g -> case f of
      New k ->
        let (sc, g') = addScope g
        in k sc g'
      Edge sc l sc' k ->
        case addEdge g sc l sc' of
          Left s -> err s
          Right g' -> k g'
      Sink sc l d k ->
        case addSink g sc l d of
          Left s -> err s
          Right g' -> k g'
      Query sc re po ad k ->
        let rs = execQuery g sc re po ad
        in k (map (\ (d,_,_) -> d) rs) g)


