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
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [a] = [a]
myReverse (x:xs) = myReverse xs ++ [x]

isPalyndrome :: (Eq a) => [a] -> Bool
isPalyndrome list = list == (myReverse list)
isPalyndrome' [] = True
isPalyndrome' [_] = True
isPalyndrome' list = (head list) == (last list) && isPalyndrome (tail list)

data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem elem) = [elem]
myFlatten (List elem) = concatMap myFlatten elem

myCompress :: (Eq a) => [a] -> [a]
myCompress list = map head $ group list

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (first:list) = let (processed,rest) = span (\a -> a == first) list
               in (first:processed) : pack rest

--encode :: [a] -> [(Int, a)]
--encode [] = []
--encode [a] = [(1, a)]
--encode list = (pack list)