module Lists
( NestedList,
  lastElem,
  sndLastElem,
  nthElem,
  numElem,
  reverseLst,
  isPalindrome,
  flatten,
  rmDuplicate,
  packLst,
  encodeLst
) where

--Problem 1: Find the last element of a list.
lastElem :: [a] -> a
lastElem [] = error "Empty List!"
lastElem [x] = x
lastElem (_ : xs) = lastElem xs

--Problem 2: Find the last but one element of a list.
sndLastElem :: [a] -> a
sndLastElem xs
  | length xs < 2 = error "List Too Small"
sndLastElem [x, _] = x
sndLastElem (_ : xs) = sndLastElem xs

--Problem 3: Find the N'th element of a list. The first element in the list is number 1.
nthElem :: [a] -> Int -> a
nthElem xs n
  | length xs < n = error "List Too Small"
  | n == 1 = head xs
  | otherwise = nthElem (tail xs) (n - 1)

--Problem 4: Find the number of elements of a list.
numElem :: [a] -> Int
numElem [] = 0
numElem (_ : xs) = 1 + numElem xs

--Problem 5: Reverse a list.
reverseLst :: [a] -> [a]
reverseLst [x] = [x]
reverseLst (x : xs) = reverseLst xs ++ [x]

--Problem 6: Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome lst
  | head lst == last lst = isPalindrome . tail . init $ lst
  | otherwise = False

--Problem 7: Flatten a nested list structure.
--Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
--We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = foldl (\acc x -> acc ++ flatten x) [] xs

--Problem 8: Eliminate consecutive duplicates of list elements.
rmDuplicate :: (Eq a) => [a] -> [a]
rmDuplicate [] = []
rmDuplicate [x] = [x]
rmDuplicate xs
  | head xs == head (tail xs) = rmDuplicate (tail xs)
  | otherwise = head xs : rmDuplicate (tail xs)

--Problem 9: Pack consecutive duplicates of list elements into sublists.
packLst :: (Eq a) => [a] -> [[a]]
packLst [] = []
packLst [x] = [[x]]
packLst xs = reverse $ foldl (\acc x -> if x `isIn` acc then (x : head acc) : tail acc else [x] : acc) [] xs where
  isIn :: (Eq a) => a -> [[a]] -> Bool
  isIn a ls
    | null ls = False
    | otherwise = a `elem` head ls

--Problem 10: Run-length encoding of a list.
--Implement the so-called run-length encoding data compression method.
--Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encodeLst :: (Eq a) => [a] -> [(Int, a)]
encodeLst [] = []
encodeLst xs = process $ foldl (\acc x -> if x `isIn` acc then (x : head acc) : tail acc else [x] : acc) [] xs where
  isIn :: (Eq a) => a -> [[a]] -> Bool
  isIn a ls
    | null ls = False
    | otherwise = a `elem` head ls
  process :: [[a]] -> [(Int, a)]
  process = foldl (\acc x -> (length x, head x):acc) []
