module LogicAndCodes where


-- 46 Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
and' :: Bool -> Bool -> Bool
and' True True = True
and' _    _    = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

nor' :: Bool -> Bool -> Bool
nor' a b = not' (or' a b)

xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True

impl' :: Bool -> Bool -> Bool
impl' a b = (not' a) `or'` b

equ' :: Bool -> Bool -> Bool
equ' a b = a == b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b ) | a <- [True, False], b<- [True, False]]

-- 42 Truth tables for logical expression 2
infixl 1 `not'`
infixl 2 `equ'`
infixl 3 `and'`
infixl 4 `xor'`
infixl 5 `or'`