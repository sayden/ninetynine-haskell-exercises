module BinaryTrees where

--import Data.Tree

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

-- 55 Construct completely balanced binary trees
cbalTree 0 = Empty
cbalTree n | n > 1 = Branch n (cbalTree (n-1)) (cbalTree (n-1))
           | otherwise = Branch n Empty Empty


-- 56 Symmetric binary trees
symmetric Empty = True
symmetric (Branch _ a b) = mirror a b

mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ x y) = mirror a b && mirror x y
mirror _ _ = False