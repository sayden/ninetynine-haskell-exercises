module ListsContinued where

import Data.List
import NNLists.NNLists

--11 Modified run length encoding
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