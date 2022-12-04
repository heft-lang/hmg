module Free.Read where

import Free

data Read r k = Read (r -> k)
  deriving Functor

read :: Read r < f => Free f r
read = Do $ inj $ Read Pure
