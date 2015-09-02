module NNLists.NNListsAgain where

import Data.List
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
