module Typeclasses.DictionaryEq where

import qualified Prelude ((==))
import           Prelude hiding (Eq, (==))

data Eq a = MkEq (a -> a -> Bool)
(==) (MkEq eq) = eq

dEqBool :: Eq Bool
dEqBool = MkEq (\x y -> if x then y else not y)

dEqList :: Eq a -> Eq [a]
dEqList d = MkEq eqlist
    where eqlist [] [] = True
          eqlist (x:xs) (y:ys) = (==) d x y && eqlist xs ys
          eqlist _ _ = False

-- TODO dEqPair
-- dEqPair :: Eq a -> Eq b -> Eq (a,b)
-- dEqPair d1 d2 = MkEq eqpair
--     where eqpair () () = True
