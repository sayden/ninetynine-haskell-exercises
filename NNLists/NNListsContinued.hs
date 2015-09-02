module NNLists.NNListsContinued where

import Data.List
import NNLists.NNLists

--11-13 Modified run length encoding
data RunLength a = Single a | Comp (Int, a)
    deriving (Show)

modifiedEncoded :: (Eq a) => [a] -> [RunLength a]
modifiedEncoded list = map complexPack (group list)
                where
                    complexPack reducedList =
                        if (length reducedList) == 1
                        then Single (head reducedList)
                        else Comp((length reducedList), (head reducedList))

--11 Modified using previous encode function
modifiedEncodedPatternMatching :: (Eq a) => [a] -> [RunLength a]
modifiedEncodedPatternMatching xs = map complexPack $ encode xs
                        where
                            complexPack (1,a) = Single a
                            complexPack (n,a) = Comp(n,a)

--12
decodeModified :: [RunLength a] -> [a]
decodeModified a = concatMap unpack a
                where
                    unpack (Single a) = [a]
                    unpack (Comp (n, a)) = replicate n a

--14 Duplicate elements in list
dupli :: [a] -> [a]
dupli list = concatMap (\x -> x:x:[]) list


--15 Replicate the elements of a list a given number of times
repli :: Int -> [a] -> [a]
repli n list = concatMap (replicate n) list

--16 Drop every N'th element from a list.
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop i xs = concat $ (take (i-1) xs) : (myDrop i (drop i xs)) : []

--17