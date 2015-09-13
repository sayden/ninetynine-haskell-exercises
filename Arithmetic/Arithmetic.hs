module Arithmetic.Arithmetic where

-- 31 Determine whether a given integer number is prime.
divisors :: Int -> [Int]
divisors x = filter (\a -> (mod x a) == 0) candidates
    where
        squarer = round $ sqrt (fromIntegral x)
        candidates = takeWhile (<squarer) [1..x-1]

isPrime :: Int -> Bool
isPrime x = length divisors' == 1
    where
        divisors' = take 2 $ divisors x


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

-- 35
--primeFactors :: Integer -> [Integer]
--primeFactors x ==
--primeFactors x
--            | isPrime x = x
--            | otherwise = primeFactors $ x / (findFirstDivisor x)
--                where
--                    findFirstDivisor y z = find (\x -> mod y x == 0) [2..y-1]