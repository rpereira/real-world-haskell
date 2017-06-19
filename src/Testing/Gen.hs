module Testing.Gen where

import Control.Monad   (filterM, replicateM)
import Test.QuickCheck

-- | Generates a list of random length. The maximum length depends on the size
-- parameter.
--
-- Usage example:
--     > sample (listOf' $ elements "aeiou")
--     ""
--     ""
--     "i"
--     "iaaaai"
--     "aauaauee"
--     "ioai"
--     "aeu"
--     "eauu"
--     "uuaiea"
--     "iuooeiuiouo"
--     "eeoaeaoeeooiieio"
listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n -> do
    k <- choose (0,n)
    vectorOf' k gen

-- | Generates a non-empty list of random length. The maximum length depends on
-- the size parameter.
--
-- Usage example:
--     > sample (listOf'' $ elements "aeiou")
--     "i"
--     "ua"
--     "aoui"
--     "uue"
--     "a"
--     "eeueaaiau"
--     "oe"
--     "oau"
--     "iueuiuouaaeeaaea"
--     "aouiaua"
--     "oauuai"
listOf'' :: Gen a -> Gen [a]
listOf'' gen = sized $ \n -> do
    k <- choose (1, 1 `max` n)
    vectorOf' k gen

-- | Generates a list of the given length.
--
-- Usage example:
--     > sample (vectorOf' 5 $ elements "aeiou")
--     "aaoeu"
--     "eaiiu"
--     "aeaaa"
--     "aaiee"
--     "aoaeo"
--     "aauoo"
--     "aiiii"
--     "iiaiu"
--     "iaaaa"
--     "eaaai"
--     "aooaa"
vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' = replicateM

-- | Generates an infinite list.
--
-- Usage example:
--     > sample (infiniteListOf' $ elements "aeiou")
--     uieauaaeioeioieooeaioiaooueoiuoe...
infiniteListOf' :: Gen a -> Gen [a]
infiniteListOf' gen = sequence (repeat gen)

-- | Randomly uses one of the given generators. The input list
-- must be non-empty.
oneOf' :: [Gen a] -> Gen a
oneOf' [] = error "Cannot be used with empty list"
oneOf' gs = choose (0, length gs - 1) >>= (gs !!)

-- | Generates a random subsequence of the given list.
--
-- Usage example:
--     > sample (subListOf' [1..10])
--     [1,2,6,7,8,10]
--     [1,4,7,8,10]
--     [1,2,3,5,6,7]
--     [3,4,6,7,9,10]
--     [1,2,3,6,7,8,10]
--     [2,4,6,7,9,10]
--     [1,2,5,8]
--     [2,3,4,5,10]
--     [1,4,5,8,9,10]
--     [1,7,8,10]
--     [1,2,3,4,5,7,8,9,10]
subListOf' :: [a] -> Gen [a]
subListOf' xs = filterM (\_ -> choose (False, True)) xs
