module H99.Codes where

import Control.Monad (replicateM)
import Data.Bits (Bits, shiftR, xor)
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

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

-- Problem #49
-- Give the sequence of n-bit Gray code, as a sequence of n-characters strings.

-- toGray(n) = n xor (floor(n/2))
toGray :: (Show a, Integral a, Bits a) => a -> a
toGray n = xor n $ shiftR n 1

grayCode :: (Show a, Integral a, Bits a) => a -> [String]
grayCode n = map (addLeadingZeros . toBinaryString . toGray) [0..n]
    where
        toBinaryString n = showIntAtBase 2 intToDigit n ""
        addLeadingZeros xs = replicate (digitsToShow - length xs) '0' ++ xs
        digitsToShow = if n < 2 then 1 else (ceiling $ logBase 2 $ fromIntegral n)

gray :: (Integral a) => a -> [String]
gray n = grayCode $ toInteger $ 2^n - 1
