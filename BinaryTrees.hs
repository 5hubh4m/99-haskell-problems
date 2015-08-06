module BinaryTrees
( Tree,
  leaf,
  compBalTree,
  isSymmTree,
  lst2Tree
) where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

--Problem 55: Construct completely balanced binary trees
--In a completely balanced binary tree, the following property holds for every Branch: The number of Branchs in its left subtree and the number of Branchs in its right subtree are almost equal, which means their difference is not greater than one.
compBalTree :: a -> Int -> Tree a
compBalTree _ 0 = Empty
compBalTree x n
  | even n = Branch x (compBalTree x $ (n - 1) `quot` 2) (compBalTree x $ (n + 1) `quot` 2)
  | odd n = Branch x (compBalTree x $ n `quot` 2) (compBalTree x $ n `quot` 2)
  | otherwise = error "Invalid Input"

--Problem 56: Symmetric binary trees
--Let us call a binary tree symmetric if you can draw a vertical line through the root Branch and then the right subtree is the mirror image of the left subtree.
--Write a predicate symmetric/1 to check whether a given binary tree is symmetric.
isSymmTree :: Tree Int -> Bool
isSymmTree x = process x == process (invertTree x) where
  invertTree :: Tree a -> Tree a
  invertTree Empty = Empty
  invertTree (Branch n y z) = Branch n (invertTree z) (invertTree y)
  process :: Tree Int -> Tree Int
  process Empty = Empty
  process (Branch _ a b) = Branch 0 (process a) (process b)

--Problem 57: Binary search trees (dictionaries)
--Write a predicate to construct a binary search tree from a list of integer numbers.
lst2Tree :: (Ord a) => [a] -> Tree a
lst2Tree [] = Empty
lst2Tree xs = process xs Empty where
  insertTree :: (Ord a) => a -> Tree a -> Tree a
  insertTree n Empty = leaf n
  insertTree n (Branch m p q) = if m > n then Branch m (insertTree n p) q else Branch m p (insertTree n q)
  process :: (Ord a) => [a] -> Tree a -> Tree a
  process y a = foldl (flip insertTree) a y
