module Free.Scope where

-- import Debug.Trace

import Free
import Free.Error

import Data.Regex
import Data.List

-- Lists

lookupAll :: Eq a => a -> [(a, b)] -> [b]
lookupAll key = map snd . filter ((== key) . fst)

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

-- Operations

data Scope s l d k
  = New (s -> k)
  | Edge s l s k
  | Sink s l d k
  | Query s (RE l) (ResolvedPath l d -> ResolvedPath l d -> Bool) (d -> Bool) ([d] -> k)
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
     => s -> RE l -> (ResolvedPath l d -> ResolvedPath l d -> Bool) -> (d -> Bool) -> Free f [d]
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

execQuery :: ( Show d , Show l , Eq l )
          => Graph l d
          -> Sc
          -> RE l
          -> (ResolvedPath l d -> ResolvedPath l d -> Bool)
          -> (d -> Bool)
          -> (Graph l d, [ResolvedPath l d])
execQuery g sc re po ad =
  let (g', ps) = findAll g re ad (Start sc)
  -- in (g', shortest po ps)
  in (g', ps)
  where
    shortest :: (ResolvedPath l d -> ResolvedPath l d -> Bool) -> [ResolvedPath l d] -> [ResolvedPath l d]
    shortest _  [] = []
    shortest po (dp:dps) = go dp [] dps
      where
        go p a [] = p:a
        go p a (p':dps) =
          if p' `po` p
          then if po p p'
               then go p (p':a) dps
               else go p' [] dps
          else go p a dps

    resolveLbl :: (Eq l, Show l, Show d)
               => ScopePath l
               -> RE l
               -> (d -> Bool)
               -> l
               -> Graph l d
               -> (Graph l d, [ResolvedPath l d])
    resolveLbl p re_old ad l g =
      let sc = dst p
          re = derive l re_old
          -- declarations in current scope
          sinks = if possiblyEmpty re then map (ResolvedPath p l) . filter ad . lookupAll l $ sinksOf g $ dst p else []
          -- targets of traversible edges (filtering scopes in `p`: prevent cycles)
          tgts = filter (not . flip inPath p) . lookupAll l $ edgesOf g $ dst p
          -- close 
          g' = g { clos = \ sc' -> if sc == sc' then l : clos g sc else clos g sc' }
          -- results of residual queries over `tgts`
          (g'', r) = foldr (\s (g, p') ->
                              let (g', p'') = findAll g re ad (Step p l s) in
                                (g', p' ++ p'')
                           )
                           (g', sinks)
                           tgts
                           in
          (g'', r)

    findAll :: (Show l, Show d, Eq l)
            => Graph l d
            -> RE l
            -> (d -> Bool)
            -> ScopePath l
            -> (Graph l d, [ResolvedPath l d])
    findAll g re ad p = foldr ( \l (g, p') ->
                                  let (g', p'') = resolveLbl p re ad l g in
                                    (g', p' ++ p'')
                              )
                              (g, [])
                              (frontier re)


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


