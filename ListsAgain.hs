module ListsAgain
( insertLst,
  mkRange,
  rndSelect,
  lotto,
  rndPermLst,
  combination,
  genPowerSet,
--  genGroup,

) where

import ListsContinued
import System.Random
import System.IO.Unsafe

--Problem 21: Insert an element at a given position into a list.
insertLst :: a -> Int -> [a] -> [a]
insertLst x n xs
  | n > length xs || n < 1 = error "Invalid Argument"
  | otherwise = take (n - 1) xs ++ [x] ++ drop (length xs - n - 1) xs

--Problem 22: Create a list containing all integers within a given range.
mkRange :: Int -> Int -> [Int]
mkRange a b
  | a == b = [a]
  | otherwise = a : mkRange (a + 1) b

--Problem 23: Extract a given number of randomly selected elements from a list.
--This solution presently uses unsafePerformIO which should be replaced in future.
rndSelect :: Int -> [a] -> [a]
rndSelect _ [] = []
rndSelect 0 _ = []
rndSelect n xs
  | n > length xs || n < 0 = error "Invalid Argument"
  | otherwise = recur $ removeNth (unsafePerformIO (randomIO :: IO Int) `mod` length xs + 1) xs where
  recur :: (a, [a]) -> [a]
  recur (a, ys) = a : rndSelect (n - 1) ys

--Problem 24: Lotto: Draw N different random numbers from the set 1..M.
lotto :: Int -> Int -> [Int]
lotto n m = rndSelect n (mkRange 1 m)

--Problem 25: Generate a random permutation of the elements of a list.
rndPermLst :: [a] -> [a]
rndPermLst xs = rndSelect (length xs) xs

--Problem 26: Generate the combinations of K distinct objects chosen from the N elements of a list
combination :: Int -> [a] -> [[a]]
combination 0 _ = [[]]
combination _ [] = []
combination n xs = map (head xs :) (combination (n - 1) (tail xs)) ++ combination n (tail xs)

--Problem 27: Group the elements of a set into disjoint subsets.
--Part A: Equivalent to generating the power set
genPowerSet :: [a] -> [[a]]
genPowerSet xs = genSet (length xs) xs where
  genSet :: Int -> [a] -> [[a]]
  genSet 0 _ = []
  genSet n ys = combination n ys ++ genSet (n - 1) ys

--Part B: Specify a list of group sizes to produce a list of groups.
--Can't wrap my head around this problem just yet
