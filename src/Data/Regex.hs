module Data.Regex where

import Data.List

data RE l
  = Empty              -- empty language (accepting state)
  | Stuck              -- invalid language (rejecting state)
  | Atom l
  | Pipe (RE l) (RE l)
  | Dot (RE l) (RE l)
  | Star (RE l)
  deriving Show

-- Computes the list of valid "next" atoms
frontier :: Eq l => RE l -> [l]
frontier Empty = []
frontier Stuck = []
frontier (Atom l) = [l]
frontier (Pipe r1 r2) = nub $ frontier r1 ++ frontier r2
frontier (Dot r1 r2) = if possiblyEmpty r1
  then if definitelyEmpty r1
       then frontier r2
       else frontier r1 ++ frontier r2
  else frontier r1
frontier (Star r) = frontier r

-- Check if regular expression is possibly the empty language
possiblyEmpty :: RE l -> Bool
possiblyEmpty Empty = True
possiblyEmpty Stuck = False
possiblyEmpty (Atom _) = False
possiblyEmpty (Pipe r1 r2) = possiblyEmpty r1 || possiblyEmpty r2
possiblyEmpty (Dot r1 r2) = possiblyEmpty r1 && possiblyEmpty r2
possiblyEmpty (Star _) = True

-- Check if regular expression is definitely the empty language
definitelyEmpty :: RE l -> Bool
definitelyEmpty Empty = True
definitelyEmpty Stuck = False
definitelyEmpty (Atom _) = False
definitelyEmpty (Pipe r1 r2) = definitelyEmpty r1 && definitelyEmpty r2
definitelyEmpty (Dot r1 r2) = definitelyEmpty r1 && definitelyEmpty r2
definitelyEmpty (Star r) = definitelyEmpty r


-- Brzozowski derivative of regular expression
derive :: Eq l => l -> RE l -> RE l
derive _ Empty = Stuck
derive _ Stuck = Stuck
derive l (Atom l') | l == l' = Empty
                   | otherwise = Stuck
derive l (Pipe r1 r2) = case (derive l r1, derive l r2) of
  (Stuck, r2') -> r2'
  (r1', Stuck) -> r1'
  (r1', r2')   -> Pipe r1' r2'
derive l (Dot r1 r2) = if definitelyEmpty r1
  then derive l r2
  else Dot (derive l r1) r2
derive l (Star r) = Dot (derive l r) (Star r)

