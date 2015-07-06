module ListsContinued
( ListElement,
  encodeLstProper,
  decodeLst,
  encodeLstDirect,
  duplicateLst,
  replicateLst,
  dropEveryNth,
  splitLst,
  sliceLst,
  rotateLst,
  removeNth
) where

import Lists

--Problem 11: Modified run-length encoding.
--Modify the result of Problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
--Only elements with duplicates are transferred as (N E) lists.
data ListElement a = Single a | Multiple Int a deriving Show

encodeLstProper :: (Eq a) => [a] -> [ListElement a]
encodeLstProper = map proc . encodeLst  where
  proc :: (Int, a) -> ListElement a
  proc (1, a) = Single a
  proc (x, a) = Multiple x a

--Problem 12: Decode a run-length encoded list.
--Given a run-length code list generated as specified in Problem 11.
--Construct its uncompressed version.
decodeLst :: (Eq a) => [ListElement a] -> [a]
decodeLst = concatMap proc' where
  proc' :: ListElement a -> [a]
  proc' (Multiple x a) = replicate x a
  proc' (Single a) = [a]

--Problem 13: Run-length encoding of a list (direct solution).
--Implement the so-called run-length encoding data compression method directly.
--Don't explicitly create the sublists containing the duplicates, as in Problem 9, but only count them.
--As in Problem 11, simplify the result list by replacing the singleton lists (1 X) by X.
encodeLstDirect :: (Eq a) => [a] -> [ListElement a]
encodeLstDirect = reverse . foldl (\acc x -> if x  `isFstElemOf` acc then plusOne acc else Single x : acc) [] where
  isFstElemOf :: (Eq a) => a -> [ListElement a] -> Bool
  isFstElemOf x acc = not (null acc) && (x == fstElem (head acc)) where
    fstElem (Multiple _ a) = a
    fstElem (Single a) = a
  plusOne :: [ListElement a] -> [ListElement a]
  plusOne [] = []
  plusOne xs = addOne (head xs) : tail xs where
    addOne :: ListElement a -> ListElement a
    addOne (Single a) = Multiple 2 a
    addOne (Multiple n a) = Multiple (n + 1) a

--Problem 14: Duplicate the elements of a list.
duplicateLst :: [a] -> [a]
duplicateLst = reverse . foldl (\acc x -> x : x : acc) []

--Problem 15: Replicate the elements of a list a given number of times.
replicateLst :: Int -> [a] -> [a]
replicateLst n = reverse . foldl (\acc x -> replicate n x ++ acc) []

--Problem 16: Drop every N'th element from a list.
dropEveryNth :: Int -> [a] -> [a]
dropEveryNth n = reverse . fst . foldl (\acc x -> if snd acc `mod` n /= 0 then (x : fst acc, snd acc + 1) else (fst acc, snd acc + 1)) ([], 1)

--Problem 17: Split a list into two parts; the length of the first part is given.
splitLst :: Int -> [a] -> ([a], [a])
splitLst n = takeout . foldl (\acc x -> if thd acc < n then (x : fst' acc, snd' acc, thd acc + 1) else (fst' acc, x : snd' acc, thd acc + 1)) ([], [], 0) where
  fst' :: (a, b, c) -> a
  fst' (x, _, _) = x
  snd' :: (a, b, c) -> b
  snd' (_, x, _) = x
  thd :: (a, b, c) -> c
  thd (_, _, x) = x
  takeout :: ([a], [a], Int) -> ([a], [a])
  takeout (a, b, _) = (reverse a, reverse b)

--Problem 18: Extract a slice from a list.
--Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
--Start counting the elements with 1.
sliceLst :: Int -> Int -> [a] -> [a]
sliceLst a b = reverse . fst . foldl (\acc x -> if snd acc >= a && snd acc <= b then (x : fst acc, snd acc + 1) else (fst acc, snd acc + 1)) ([], 1)

--Problem 19: Rotate a list N places to the left.
rotateLst :: Int -> [a] -> [a]
rotateLst 0 xs = xs
rotateLst n xs
  | n < 0 = rotateLst (length xs + n) xs
  | n > length xs = rotateLst (n `mod` length xs) xs
  | otherwise = rotateLst (n - 1) (tail xs ++ [head xs])

--Problem 20: Remove the N'th element from a list.
removeNth :: Int -> [a] -> (a, [a])
removeNth n = take' . foldl (\acc x -> if thd acc == n then ([x], snd' acc, thd acc + 1) else (fst' acc, x : snd' acc, thd acc + 1)) ([], [], 1) where
  fst' :: (a, b, c) -> a
  fst' (x, _, _) = x
  snd' :: (a, b, c) -> b
  snd' (_, x, _) = x
  thd :: (a, b, c) -> c
  thd (_, _, x) = x
  take' :: ([a], [a], Int) -> (a, [a])
  take' (xs, ys, _) = (head xs, reverse ys)
