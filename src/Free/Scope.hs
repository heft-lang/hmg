module Free.Scope where

-- import Debug.Trace

import Free
import Free.Error

import Data.Regex
import Data.List

-- Lists/Tuples

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll key = map snd . filter ((== key) . fst)

mapSnd :: (b -> c) -> (a,b) -> (a,c)
mapSnd f (a,b) = (a, f b)

-- ScopePaths

data ScopePath l
  = Start Sc
  | Step (ScopePath l) l Sc
  deriving (Eq, Show)

dst :: ScopePath l -> Sc
dst (Start    s) = s
dst (Step _ _ s) = s

inPath :: Sc -> ScopePath l -> Bool
inPath sc (Start sc') = sc == sc'
inPath sc (Step p _ sc') = sc == sc' || inPath sc p

lenPath :: ScopePath l -> Int
lenPath (Start _) = 0
lenPath (Step p _ _) = 1 + lenPath p

data ResolvedPath l d
  = ResolvedPath (ScopePath l) l d
  deriving (Eq, Show)

dataOfPath :: ResolvedPath l d -> d
dataOfPath (ResolvedPath _ _ d) = d

lenRPath :: ResolvedPath l d -> Int
lenRPath (ResolvedPath p _ _) = lenPath p

type PathOrder l d = ResolvedPath l d -> ResolvedPath l d -> Bool

-- Operations

data Scope s l d k
  = New (s -> k)
  | Edge s l s k
  | Sink s l d k
  | Query s (RE l) (PathOrder l d) (d -> Bool) ([d] -> k)
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
     => s -> RE l -> PathOrder l d -> (d -> Bool) -> Free f [d]
query s re po ad = Do $ inj $ Query s re po ad Pure


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
            else Right $ rawAdd g s l s'
            --   let edges = entries g s
            -- in if null edges
            --    then Right $ 
            --    else case lookup l edges of
            --           Nothing -> Right $ rawAdd g s l s'
            --           Just _ ->
            --             Left $ "Error: scope " ++ show s ++ " already has an edge labeled " ++ show l
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
sinksOf g sc = concatMap (\ (l, e) -> either
                     (const [])
                     (\ e -> [(l, e)])
                     e) (entries g sc)

edgesOf :: Graph l d -> Sc -> [(l, Sc)]
edgesOf g sc = concatMap (\ (l, e) -> either
                     (\ sc' -> [(l, sc')])
                     (const [])
                     e) (entries g sc)

execQuery :: ( Show d , Show l , Eq l, Eq d )
          => Graph l d
          -> Sc
          -> RE l
          -> PathOrder l d
          -> (d -> Bool)
          -> (Graph l d, [ResolvedPath l d])
execQuery g sc re po ad = mapSnd (shortest po) $ findAll g re ad $ Start sc
  where
    -- derived from https://hackage.haskell.org/package/partial-order-0.2.0.0/docs/src/Data.PartialOrd.html#minima
    shortest :: (Eq l, Eq d) => PathOrder l d -> [ResolvedPath l d] -> [ResolvedPath l d]
    shortest po ps = nub $ filter isExtremal ps
      where
        isExtremal p = not $ any (`po` p) $ filter (/= p) ps

    resolveLbl p re_old ad l g =
      let sc = dst p
          re = derive l re_old
          -- declarations in current scope
          sinks = if possiblyEmpty re then map (ResolvedPath p l) . filter ad . lookupAll l $ sinksOf g $ dst p else []
          -- targets of traversible edges (filtering scopes in `p`: prevent cycles)
          tgts = filter (not . flip inPath p) . lookupAll l $ edgesOf g $ dst p
          -- close `l` in `g` if not already closed
          g' = if l `elem` clos g sc
               then g
               else g { clos = \ sc' -> if sc == sc'
                                        then l : clos g sc
                                        else clos g sc'
                      }
      in  -- results of residual queries over `tgts`
          foldr find (g', sinks) tgts
        where
          find s (g, p') = mapSnd (p' ++) $ findAll g re ad $ Step p l s

    findAll g re ad p = foldr find (g, []) $ frontier re
      where
        find l (g, p') = mapSnd (p' ++) $ resolveLbl p re ad l g

hScope :: ( Eq l, Show l
          , Eq d, Show d
          , Functor f
          , Error String < f )
       => Handler_ (Scope Sc l d) a (Graph l d) f (a, Graph l d)
hScope = Handler_
  (curry return)
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
        in k (map dataOfPath rs) g')


