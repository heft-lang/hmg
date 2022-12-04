module Val.Subtype where

class v ⊂ w where
  injV :: v -> w
