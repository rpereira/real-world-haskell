module Typeclasses.Eq where

import           Data.List
import qualified Prelude ((==))
import           Prelude hiding (Eq, (==))

class Eq a where
    (==) :: a -> a -> Bool

instance Eq Bool where
    False == False = True
    True  == True  = True
    _     == _     = False

instance Eq a => Eq [a] where
    []     == []     = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _      == _      = False

instance Eq Int where
    (==) = (Prelude.==)

instance (Eq a1, Eq a2) => Eq (a1, a2) where
    (x1, x2) == (y1, y2) = (x1 == y1) && (x2 == y2)

member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (y:ys) = x == y || member x ys
--
-- alternative 1:
-- member x = foldr ((||) . (==) x) False
--
-- alternative 2:
-- member x [] = False
-- member x (y:ys) = if x == y then True else member x ys

prop_ordered xs = ordered (sort xs)
    where ordered (x:y:xs) = x <= y && ordered (y:xs)
          ordered _ = True
