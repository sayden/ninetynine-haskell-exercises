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