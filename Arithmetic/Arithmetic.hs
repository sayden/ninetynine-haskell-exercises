module Arithmetic.Arithmetic where

-- 31 Determine whether a given integer number is prime.
--isPrime :: Num -> Bool
--isPrime n = [n | ]

-- 32 Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
lastMod :: Integer -> Integer -> Integer
lastMod x 0 = abs x
lastMod x y = lastMod y (mod x y)

