module Data.Regex where

import Data.List

data RE l
  = Empty              -- empty language (accepting state)
  | Stuck              -- invalid language (rejecting state)
  | Atom l
  | Pipe (RE l) (RE l)
  | Dot (RE l) (RE l)
  | Star (RE l)

-- Computes the list of valid "next" atoms
frontier :: Eq l => RE l -> [l]
frontier Empty = []
frontier Stuck = []
frontier (Atom l) = [l]
frontier (Pipe r1 r2) = nub $ frontier r1 ++ frontier r2
frontier (Dot r1 r2) = if isEmpty r1
  then frontier r2
  else frontier r1
frontier (Star r) = frontier r

-- Check if regular expression corresponds to the empty language
isEmpty :: RE l -> Bool
isEmpty Empty = True
isEmpty Stuck = False
isEmpty (Atom _) = False
isEmpty (Pipe r1 r2) = isEmpty r1 || isEmpty r2
isEmpty (Dot r1 r2) = isEmpty r1 && isEmpty r2
isEmpty (Star _) = True

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
derive l (Dot r1 r2) = if isEmpty r1
  then derive l r2
  else Dot (derive l r1) r2
derive l (Star r) = Dot (derive l r) (Star r)

