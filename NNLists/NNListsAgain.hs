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

--fullCombinations :: Int -> [a] -> [[a]]
--fullCombinations 0 _ = [[]]
--fullCombinations n x:list =

test :: Int -> [a] -> [[a]]
test 0 _ = [[]]
test n (a:b) = map (\x -> a:x:[]) b ++ test (n-1) b