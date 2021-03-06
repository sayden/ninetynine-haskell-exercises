module Arithmetic.Arithmetic where

import Data.List

-- 31 Determine whether a given integer number is prime.
divisors :: Integral a => a -> [a]
divisors x = divisors' ++ [x]
    where
        squarer = round $ (fromIntegral x) / 2
        candidates = takeWhile (<=squarer) [1..x]
        divisors' = filter (\a -> (mod x a) == 0) candidates

isPrime :: Int -> Bool
isPrime x = length result == 1
    where
        squarer = round $ sqrt (fromIntegral x)
        candidates = takeWhile (<=squarer) [1..x-1]
        divisors' = filter (\a -> (mod x a) == 0) candidates
        result = take 2 $ divisors'


-- 32 Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
lastMod :: Integer -> Integer -> Integer
lastMod x 0 = abs x
lastMod x y = lastMod y (mod x y)

-- 33 Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Integer -> Integer -> Bool
coprime a b = lastMod a b == 1

-- 34 Calculate Euler's totient function phi(m).
totientPhi :: Integer -> Int
totientPhi x = length $ filter (\a -> coprime a x) [1..x-1]

-- 35  Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order
findFirstDivisor :: Integral a => a -> a
findFirstDivisor a = isOk res
        where
            res = find (\b -> mod a b == 0) [2..a]
            isOk Nothing = 1
            isOk (Just a) = a

primeFactors' :: Integral a => a -> a -> [a]
primeFactors' d 1 = []
primeFactors' d r = [divisor] ++ result
    where
        divisor = findFirstDivisor r
        division = div r (fromIntegral divisor)
        result = primeFactors' divisor division

primeFactors :: Integral a => a -> [a]
primeFactors d = primeFactors' 1 d

-- 36 Determine the prime factors of a given number and their multiplicity
primeFactorsMult :: Integral a => a -> [(a, Int)]
primeFactorsMult a = result
    where
        factors = primeFactors a
        sorted = sort factors
        grouped = group sorted
        result = map (\x -> ((head x), (length x))) grouped

-- 37 Calculate Euler's totient function phi(m) (improved).
phi a = foldl (*) 1 res
    where
    primeF = primeFactorsMult a
    totienMult p m = (p - 1) * p ** (m - 1)
    res = map (\x -> totienMult (fromIntegral (fst x)) (fromIntegral (snd x))) primeF

-- 39 A List of primes numbers
primesR :: Int -> Int -> [Int]
primesR x y = filter (isPrime) [x..y]

-- 40 Goldbach's conjecture.
goldbach :: Int -> [(Int, Int)]
goldbach x = result
    where
        primesList = primesR 2 (x-1)
        candidates = map (\a -> (a, x - a)) primesList
        result' = filter (\(x,y) -> isPrime x && isPrime y ) candidates
        ordered = map (\(x,y) -> if x>y then (x,y) else (y,x)) result'
        sorted = sort ordered
        grouped = group sorted
        result = map (head) grouped

-- 41 Given a range of ints, print a list of all even numbers and their Goldbach composition.
goldbachList :: Int -> Int -> [[(Int, Int)]]
goldbachList x y = map goldbach $ filter even [x..y]