module Testing.QuickCheckBasics where 

import Test.QuickCheck
import Data.List

-- quickCheck (prop_idempotent :: [Integer] -> Bool)
prop_idempotent :: Ord a => [a] -> Bool
prop_idempotent xs = sort (sort xs) == sort xs

-- quickCheck (prop_minimum :: [Integer] -> Property)
prop_minimum :: Ord a => [a] -> Property
prop_minimum xs = not (null xs) ==> head (sort xs) == minimum xs

-- quickCheck (prop_maximum :: [Integer] -> Property)
prop_maximum :: Ord a => [a] -> Property
prop_maximum xs = not (null xs) ==> last (sort xs) == maximum xs

-- quickCheck (prop_ordered :: [Integer] -> Bool)
prop_ordered :: Ord a => [a] -> Bool
prop_ordered xs = orderedAsc (sort xs)

-- quickCheck (prop_permutation :: [Integer] -> Bool)
prop_permutation :: Ord a => [a] -> Bool
prop_permutation xs = permutation xs (sort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

-- quickCheck (prop_append :: [Integer] -> [Integer] -> Property)
prop_append :: Ord a => [a] -> [a] -> Property
prop_append xs ys =
    not (null xs) ==>
    not (null ys) ==>
        head (sort (xs ++ ys)) == min (minimum xs) (minimum ys)

-- quickCheck (prop_length :: [Integer] -> Bool)
prop_length :: Ord a => [a] -> Bool
prop_length xs = length (sort xs) == length xs

-- quickCheck (prop_elem :: Integer -> [Integer] -> Bool)
prop_elem :: Ord a => a -> [a] -> Bool
prop_elem x xs = elem x (sort xs) == elem x xs

-- quickCheck (prop_insert_asc :: Integer -> Property)
prop_insert_asc :: (Show a, Ord a, Arbitrary a) => a -> Property
prop_insert_asc x = forAll orderedList $
    \xs -> orderedAsc (insert' x xs)

-- quickCheck (prop_rev :: [Integer] -> [Integer] -> Bool)
prop_rev :: Ord a => [a] -> [a] -> Bool
prop_rev xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- quickCheck (prop_rev_rev :: [Integer] -> Bool)
prop_rev_rev :: Ord a => [a] -> Bool
prop_rev_rev xs = reverse (reverse xs) == xs

--------------------------------------------------------------------------------
--  Helpers

orderedAsc :: Ord a => [a] -> Bool
orderedAsc (x:y:xs) = x <= y && orderedAsc (y:xs)
orderedAsc _        = True

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
    | x < y     = x:y:ys
    | otherwise = y:insert x ys
