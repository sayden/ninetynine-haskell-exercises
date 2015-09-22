module BinaryTrees where

--import Data.Tree

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- 55 Construct completely balanced binary trees
cbalTree n | n > 1 = Branch n (cbalTree (n-1)) (cbalTree (n-2))
           | otherwise = Empty