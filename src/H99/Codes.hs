module H99.Codes where

import Control.Monad (replicateM)

-- Problem #46
-- Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence).
-- Also write a predicate table/3 which prints the truth table of a given
-- logical expression in two variables.

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b = not $  a && b

nor' :: Bool -> Bool -> Bool
nor' a b= not $ a || b

xor' :: Bool -> Bool -> Bool
xor' a b = (a || b) && not (a && b)

impl' :: Bool -> Bool -> Bool
impl' a b = (not a) || b

equ' :: Bool -> Bool -> Bool
equ' = (==)

table :: (Bool -> Bool -> Bool) -> String
table f = concatMap (++"\n") $ display <$> [True, False] <*> [True, False]
    where
        display a b = show a ++ " " ++ show b ++ " " ++ show (f a b)

-- Problem #48
-- Truth tables for logical expressions of N variables

tableN :: Int -> ([Bool] -> Bool) -> String
tableN n f = concatMap (++"\n") $ display <$>replicateM n [True, False]
    where
        display xs = concatMap ((++" ") . show) xs ++ show (f xs)
