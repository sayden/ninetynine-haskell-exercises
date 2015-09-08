module NNLists.NNListsAgain where

import Data.List
import System.Random
import NNLists.NNListsContinued

--21 Insert an element at a given position into a list.
insertAt :: Int -> [a] -> [a] -> [a]
insertAt i a xs = before ++ a ++ after ++ []
        where
            before = take i xs
            after = drop i xs

--22 Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range a b = take (b-a+1) (iterate (+1) a)

--23 Extract a given number of randomly selected elements from a list.
rndSelect :: Int -> [a] -> IO [a]
rndSelect amount list = do
    gen <- getStdGen
    return $ take amount [ list !! x | x <- randomRs (0, (length list) - 1) gen]

--24 Draw N different random numbers from the set 1..M.
diffSelect :: Int -> Int -> IO [Int]
diffSelect amount maxNumber = do
    gen <- getStdGen
    return $ take amount (randomRs (1, maxNumber) gen)

--25 Generate a random permutation of the elements of a list.
rndPermu :: [a] -> IO [a]
rndPermu list = rndSelect (length list) list

--26 Generate the combinations of K distinct objects chosen from the N elements of a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n list = do y:xs <- tails list
                         ys <- combinations (n-1) xs
                         return (y:ys)

--27 Group the elements of a set into disjoint subsets.

--28 a) Sorting a list of lists according to length of sublists
lsort :: Ord a => [[a]] -> [[a]]
lsort list = sortBy (\a b -> compare (length a) (length b)) list

-- b) sort the elements of this list according to their length frequency
--lSortFreq :: Ord a => [[a]] -> [ListLength]
--map (\(_,b) -> b) lastList
lSortFreq list = map (\(_,a) -> a) sorted
    where
        counted = map (\x -> ((length x), x)) list
        lastList = sortBy (\(a,_) (b,_)-> compare b a) counted
        grouped = groupBy (\(a,_) (b,_) -> a==b) lastList
        sorted = concat $ sortBy (\x y -> compare (length x) (length y)) grouped