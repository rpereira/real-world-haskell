module Typeclasses.Show where

import Data.List

class MyShow a where
    myshow :: a -> String

instance MyShow Bool where
    myshow b = if b then "True" else "False"

instance (MyShow a, MyShow b) => MyShow (a,b) where
    myshow (x,y) = "(" ++ myshow x ++ "," ++ myshow y ++ ")"

instance MyShow a => MyShow [a] where
    myshow xs = "[" ++ showList xs ++ "]"
        --where showList [] = ""
        --      showList [x] = myshow x
        --      showList (x:xs) = myshow x ++ ", " ++ showList xs
        where showList = intercalate ", " . map myshow

instance MyShow () where
    myshow () = "()"

--------------------------------------------------------------------------------
--  Dictionary translation
data DictShow a = DictShow { dshow :: a -> String }

dShowBool :: DictShow Bool
dShowBool = DictShow
    { dshow = \b -> if b then "True" else "False" }

dShowPair :: DictShow a -> DictShow b -> DictShow(a,b)
dShowPair d1 d2 = DictShow
    { dshow = \(x, y) -> "(" ++ dshow d1 x ++ "," ++ dshow d2 y ++ ")" }

dShowList :: DictShow a -> DictShow [a]
dShowList d = DictShow
    { dshow = \xs -> "[" ++ showList xs ++ "]" }
        where showList [] = ""
              showList [x] = dshow d x
              showList (x:xs) = dshow d x ++ "," ++ showList xs

dShowUnit :: DictShow a -> DictShow ()
dShowUnit d = DictShow
    { dshow = \() -> "()" }
