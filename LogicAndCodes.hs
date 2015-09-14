module LogicAndCodes where

import Control.Monad (replicateM)

-- 46 Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
and' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

nor' :: Bool -> Bool -> Bool
nor' a b = not' (or' a b)

xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True

impl' :: Bool -> Bool -> Bool
impl' a b = (not' a) `or'` b

equ' :: Bool -> Bool -> Bool
equ' a b = a == b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b ) | a <- [True, False], b<- [True, False]]

-- 47 Truth tables for logical expression 2
infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

-- 48 Truth tables for logical expressions (3).

genTable :: Int -> ([Bool] -> Bool) -> IO ()
genTable n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- replicateM n [True, False]]
    where
        toStr = unwords . map (\x -> show x ++ " ")
