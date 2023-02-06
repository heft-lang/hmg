module Free.Scope where

-- import Debug.Trace

import Free
import Free.Error

import Data.Regex
import Data.List

-- Paths

data Path s l
  = Stop Sc
  | Step Sc l (Path s l)
  deriving Show

inPath :: Sc -> Path s l -> Bool
inPath sc (Stop sc') = sc == sc'
inPath sc (Step sc' _ p) = sc == sc' || inPath sc p

lenPath :: Path s l -> Int
lenPath (Stop _) = 0
lenPath (Step _ _ p) = 1 + lenPath p


-- Operations

data Scope s l d k
  = New (s -> k)
  | Edge s l s k
  | Sink s l d k
  | Query s (RE l) (Path s l -> Path s l -> Bool) (d -> Bool) ([d] -> k)
  deriving Functor

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

type Sc = Int

data Graph l d
  = Graph { scopes  :: Sc
          , entries :: Sc -> [(l, Either Sc d)]
          , clos    :: Sc -> [l] }

instance (Show l, Show d) => Show (Graph l d) where
  show g =
    intercalate
      "\n"
      [show s ++ ": " ++ show (entries g s) | s <- [0..scopes g]]

emptyGraph :: Graph l d
emptyGraph = Graph 0 (const []) (const [])

addScope :: Graph l d -> (Sc, Graph l d)
addScope g = let sc' = scopes g + 1
  in (sc', g { scopes = sc' })

addEdge :: ( Eq l
           , Show l )
        => Graph l d -> Sc -> l -> Sc -> Either String (Graph l d)
addEdge g s l s' =
  if s <= scopes g
  then if s' <= scopes g
       then if l `elem` clos g s
            then Left $ "Monotonicity error: the scope " ++ show s ++ " has already been queried for labels " ++ show l
            else let edges = entries g s
            in if (null edges)
               then Right $ rawAdd g s l s'
               else case lookup l edges of
                      Nothing -> Right $ rawAdd g s l s'
                      Just _ ->
                        Left $ "Error: scope " ++ show s ++ " already has an edge labeled " ++ show l
       else Left $ "Invalid scope: " ++ show s'
  else Left $ "Invalid scope: " ++ show s
  where
    rawAdd g s l s' =
      g { entries = \ sc -> if sc == s
                            then (l, Left s'):entries g s
                            else entries g sc }

addSink :: ( Eq l
           , Eq d
           , Show d
           , Show l )
        => Graph l d -> Sc -> l -> d -> Either String (Graph l d)
addSink g s l d =
  if s <= scopes g
  then if l `elem` clos g s
       then Left $ "Monotonicity error: the scope " ++ show s ++ " has already been queried for labels " ++ show l
       else case sinksOf g s of
         [] -> Right $ rawAdd g s l d
         sinks ->
           if (l,d) `elem` sinks
           then Left $ "Error: there is already a declaration " ++ show d ++ " at label " ++ show l
           else Right $ rawAdd g s l d
  else Left $ "Invalid scope: " ++ show s
  where
    rawAdd g s l d =
      g { entries = \ sc -> if sc == s
                            then (l, Right d):entries g s
                            else entries g sc }

sinksOf :: Graph l d -> Sc -> [(l, d)]
sinksOf g sc = concat
             $ map (\ (l, e) -> either
                     (const [])
                     (\ e -> [(l, e)])
                     e)
             $ entries g sc

edgesOf :: Graph l d -> Sc -> [(l, Sc)]
edgesOf g sc = concat
             $ map (\ (l, e) -> either
                     (\ sc' -> [(l, sc')])
                     (const [])
                     e)
             $ entries g sc

execQuery :: ( Show d , Show l , Eq l )
          => Graph l d
          -> Sc
          -> RE l
          -> (Path s l -> Path s l -> Bool)
          -> (d -> Bool)
          -> (Graph l d, [(d, l, Path s l)])
execQuery g sc re po ad =
  let (g', ps) = findAll g sc re ad (Stop sc)
  in (g', shortest po ps)
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
        
    
    findAll :: (Show l, Show d, Eq l)
            => Graph l d -> Sc -> RE l -> (d -> Bool) -> Path s l -> (Graph l d, [(d, l, Path s l)])
    findAll g sc re ad p =
      if possiblyEmpty re -- FIXME: search could/(should?) continue
      then ( g
           , map (\ (l, d) -> (d, l, p))
           $ filter (\ (_, d) -> ad d) $ sinksOf g sc )
      else let fr = frontier re
               g' = g { clos = \ sc' -> if sc == sc'
                        then fr `union` clos g sc
                        else clos g sc' }
      in foldr
           (\ l (g', rs) ->
               ( foldr
                   (\ (_, r) (g', rs') -> case r of
                       Left sc' ->
                         if inPath sc' p -- avoid cyclic paths
                         then (g', rs')
                         else
                           let (g'', rs'') = findAll g' sc' (derive l re) ad (Step sc' l p)
                           in (g'', rs' ++ rs'')
                       Right d -> if (ad d) then (g', (d, l, p):rs') else (g', rs'))
                   (g', rs)
               $ filter (\ (l', _) -> l' == l) (entries g sc) ))
           (g', [])
           fr


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
        let (g', rs) = execQuery g sc re po ad
        in k (map (\ (d,_,_) -> d) rs) g')


