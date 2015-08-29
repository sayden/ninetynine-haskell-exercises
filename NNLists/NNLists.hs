module NNLists (myLast) where

import Data.List

myLast :: [a] -> a
myLast [] = error "Error"
myLast [a] = a
myLast theList = genericIndex theList $ (length theList) - 1

myButLast :: [a] -> a
myButLast [] = error "Error"
myButLast [a] = error "Error"
myButLast theList = genericIndex theList $ (length theList) - 2

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Error"
elementAt _ 0 = error "Error"
elementAt [a] 1 = a
elementAt list index = list !! (index - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength [a] = 1
myLength list = 1 + myLength (tail list)