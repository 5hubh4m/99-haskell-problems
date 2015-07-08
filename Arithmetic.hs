module Arithmetic
( isPrime,
  gcdEuclid,
  isCoprime,
  totientPhi,
  primeFactors,
  primeFactors',
  totientPhi',
  primeInRange,
  goldBach,
  goldBachLst,
  goldBachLst'
) where

import Lists

--Problem 31: Determine whether a given integer number is prime.
isPrime :: Int -> Bool
isPrime n
  | n < 0 = error "Only Non-Negative Arguments Allowed"
  | n `elem` [0, 1] = False
  | n == 2 = True
  | otherwise = True `notElem` map (\t -> (n `mod` t) == 0) [2 .. ceiling $ sqrt $ fromIntegral n]

--Problem 32: Determine the greatest common divisor of two positive integer numbers.
--Use Euclid's algorithm.
gcdEuclid :: Int -> Int -> Int
gcdEuclid m n
  | m == 0 = n
  | n == 0 = m
  | m == n = m
  | m > n = gcdEuclid (m - n) n
  | n > m = gcdEuclid m (n - m)

--Problem 33: Determine whether two positive integer numbers are coprime.
--Two numbers are coprime if their greatest common divisor equals 1.
isCoprime :: Int -> Int -> Bool
isCoprime m n = gcdEuclid m n == 1

--Problem 34: Calculate Euler's totient function phi(m).
--Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
totientPhi :: Int -> (Int, [Int])
totientPhi m = foldr (\x acc -> if isCoprime x m then (fst acc + 1, x : snd acc) else acc) (0, []) [1 .. m-1]

--Problem 35: Determine the prime factors of a given positive integer.
--Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors m = findPrime m 2 where
  findPrime :: Int -> Int -> [Int]
  findPrime x y
    | y > x = []
    | otherwise = if x `mod` y == 0 then y : findPrime (ceiling $ (/) (fromIntegral x)  (fromIntegral y)) y else findPrime x (nextPrime y)
  nextPrime :: Int -> Int
  nextPrime n = if isPrime $ n + 1 then n + 1 else nextPrime $ n + 1

--Problem 36: Determine the prime factors of a given positive integer.
--Construct a list containing the prime factors and their multiplicity.
primeFactors' :: Int -> [(Int, Int)]
primeFactors' n = modify `map` encodeLst (primeFactors n) where
  modify :: (a, b) -> (b, a)
  modify (a, b) = (b, a)

--Problem 37: Calculate Euler's totient function phi(m) (improved).
--Use the formula:
--phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--         (p2 - 1) * p2 ** (m2 - 1) * ...
--Where p1, p2... are the prime factors of m and m1, m2... are their multiplicities.
totientPhi' :: Int -> Int
totientPhi' m = foldl (\acc x -> acc * (fst x - 1) * fst x ^ (snd x - 1)) 1 (primeFactors' m)


--Problem 39: A list of prime numbers.
--Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
primeInRange :: Int -> Int -> [Int]
primeInRange m n
  | m > n = primeInRange n m
  | m == n = [m | isPrime m]
  | otherwise = if isPrime m then m : primeInRange (m + 1) n else primeInRange (m + 1) n


--Problem 40: Goldbach's conjecture.
--Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
--Example: 28 = 5 + 23.
--It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
--Write a predicate to find the two prime numbers that sum up to a given even integer.
goldBach :: Int -> (Int, Int)
goldBach m
  | m <= 2 || odd m = error "Argument Should Be Even and Greater Than 2"
  | otherwise = check $ cart (primeInRange 2 (m- 2)) (primeInRange 2 (m - 2)) where
    check :: [(Int, Int)] -> (Int, Int)
    check [] = (0, 0)
    check (x : xs) = if uncurry (+) x == m then x else check xs
    cart :: [a] -> [a] -> [(a, a)]
    cart xs ys = [(a, b) | a <- xs, b <- ys]

--Problem 41: Goldbach's conjecture.
--Part A: Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
goldBachLst :: Int -> Int -> [(Int, Int)]
goldBachLst m n
 | m > n = goldBachLst n m
 | m == n = [goldBach m | m > 2 && even m]
 | otherwise = if m > 2 && even m then goldBach m : goldBachLst (m + 1) n else goldBachLst (m + 1) n

--Part B: In most cases, if an even number is written as the sum of two prime numbers, one of them is very small.
--Very rarely, the primes are both bigger than say 50.
--Try to find out how many such cases there are in a given range.
goldBachLst' :: Int -> Int -> Int -> [(Int, Int)]
goldBachLst' m n x = filter (\l -> fst l > x && snd l > x) (goldBachLst m n)
