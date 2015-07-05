module ListContinued
( ListElement,
  encodeLstProper,
  decodeLst,
  encodeLstDirect
) where

import Lists

data ListElement a = Single a | Multiple Int a deriving Show

encodeLstProper :: (Eq a) => [a] -> [ListElement a]
encodeLstProper = map proc . encodeLst  where
  proc :: (Int, a) -> ListElement a
  proc (1, a) = Single a
  proc (x, a) = Multiple x a

decodeLst :: (Eq a) => [ListElement a] -> [a]
decodeLst = concatMap proc' where
  proc' :: ListElement a -> [a]
  proc' (Multiple x a) = replicate x a
  proc' (Single a) = [a]

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
