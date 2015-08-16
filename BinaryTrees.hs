module BinaryTrees
( Tree,
  leaf,
  compBalTree,
  isSymmTree,
  lst2Tree,
  genTestTree,
  heightBalTree
) where

import Data.Maybe
import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

--Problem 55: Construct completely balanced binary trees.
--In a completely balanced binary tree, the following property holds for every Branch: The number of Branchs in its left subtree and the number of Branchs in its right subtree are almost equal, which means their difference is not greater than one.
compBalTree :: a -> Int -> [Tree a]
compBalTree _ 0 = [Empty]
compBalTree x n
  | odd n = [Branch x l r | l <- compBalTree x $ (n - 1) `quot` 2, r <- compBalTree x $ (n - 1) `quot` 2]
  | even n = concat [[Branch x p q, Branch x q p] | p <- compBalTree x $ (n - 1) `quot` 2, q <- compBalTree x $ n `quot` 2]
  | otherwise = []

--Problem 56: Symmetric binary trees.
--Let us call a binary tree symmetric if you can draw a vertical line through the root Branch and then the right subtree is the mirror image of the left subtree.
--Write a predicate symmetric/1 to check whether a given binary tree is symmetric.
isSymmTree :: Tree a -> Bool
isSymmTree x = isEqTree x $ invertTree x where
  invertTree :: Tree a -> Tree a
  invertTree Empty = Empty
  invertTree (Branch n y z) = Branch n (invertTree z) (invertTree y)
  isEqTree :: Tree a -> Tree b -> Bool
  isEqTree Empty Empty = True
  isEqTree Empty Branch {} = False
  isEqTree Branch {} Empty = False
  isEqTree (Branch _ a b) (Branch _ c d) = isEqTree a c && isEqTree b d

--Problem 57: Binary search trees (dictionaries).
--Write a predicate to construct a binary search tree from a list of integer numbers.
lst2Tree :: (Ord a) => [a] -> Tree a
lst2Tree [] = Empty
lst2Tree xs = process xs Empty where
  insertTree :: (Ord a) => a -> Tree a -> Tree a
  insertTree n Empty = leaf n
  insertTree n (Branch m p q) = if m > n then Branch m (insertTree n p) q else Branch m p (insertTree n q)
  process :: (Ord a) => [a] -> Tree a -> Tree a
  process y a = foldl (flip insertTree) a y

--Problem 58: Generate-and-test paradigm.
--Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with a given number of nodes.
genTestTree :: a -> Int -> [Tree a]
genTestTree x n = filter isSymmTree $ compBalTree x n

--Problem 59: Construct height balanced binary trees.
--Construct all height balanced trees for a given height.
heightBalTree :: a -> Int -> [Tree a]
heightBalTree _ 0 = [Empty]
heightBalTree x 1 = [leaf x]
heightBalTree x n = [Branch x l r | (hl, hr) <- [(n - 1, n - 2), (n - 1, n - 1), (n - 2, n - 1)], l <- heightBalTree x hl, r <- heightBalTree x hr]

--Problem 60: Construct height-balanced binary trees with a given number of nodes.
heightBalTreeNodes :: a -> Int -> [Tree a]
heightBalTreeNodes x n = concatMap filterToTrees [minHeight .. maxHeight] where
    --filterToTrees :: Int -> [Tree a]
    filterToTrees = filter ((n ==) . countNodes) . heightBalTree x
    minNodesSequence :: [Int]
    minNodesSequence = 0 : 1 : zipWith ((+) . (1 +)) minNodesSequence (tail minNodesSequence)
    minHeight :: Int
    minHeight = ceiling $ logBase 2 $ fromIntegral $ n + 1
    maxHeight :: Int
    maxHeight = fromJust (findIndex (> n) minNodesSequence) - 1
    countNodes :: Tree a -> Int
    countNodes Empty = 0
    countNodes (Branch _ l r) = countNodes l + countNodes r + 1
