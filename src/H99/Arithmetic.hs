module H99.Arithmetic where

import Data.Tuple (swap)
import H99.Lists (encode)

-- Problem #31
-- Determine whether a given integer number is prime.

isPrime :: (Integral a) => a -> Bool
isPrime 1 = False
isPrime n = all ((/= 0) . mod n) candidates
    where
        max = floor . sqrt $ fromIntegral n
        candidates = takeWhile (<= max) $ 2:[3,5..]
        -- Smaller candidates set: multiples of 6 plus or minus 1.
        -- takeWhile (<= max) $ 2:3:[ x+delta | x <-[6,12..], delta <-[-1,1] ]

-- Problem #32
-- Determine the greatest common divisor of two positive integer numbers.
-- Use Euclid's algorithm.

myGcd :: (Integral a) => a -> a -> a
myGcd a 0 = a
myGcd a b = myGcd b $ abs $ mod a b

-- Problem #33
-- Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.

coprime :: (Integral a) => a -> a -> Bool
coprime a b = (==1) $ myGcd a b

-- Problem #34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m.

totient :: (Integral a) => a -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..(n-1)]

-- Problem #35
-- Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.

primeFactors :: (Integral a) => a -> [a]
primeFactors 0 = []
primeFactors n = _primeFactors n 2
    where
        _primeFactors 1 _ = []
        _primeFactors n p
            | (n `mod` p == 0) = p : _primeFactors (n `div` p) p
            | otherwise        = _primeFactors n (p+1)

-- Problem #36
-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.

primeFactorsMult :: (Integral a) => a -> [(a, Int)]
primeFactorsMult n = map swap $ encode . primeFactors $ n

-- Problem #37
-- Calculate Euler's totient function phi(m) (improved).
--
-- Given, for an integral number N:
--      primeFactorsMult N = [(p1, m1), (p2, m2)... (pk, mk)]
-- then:
--      phi(N) = Product((pi - 1) * pi ^ (mi - 1)), i in [1..k]

totient' :: (Integral a) => a -> a
totient' 0 = 0
totient' n = foldl (*) 1 $ map (\(p, m) -> (p-1) * p^(m-1)) $ primeFactorsMult n

-- Problem #39
--  A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list of
-- all prime numbers in that range.

primesR :: (Integral a) => a -> a -> [a]
primesR a b
    | a < 2     = filter isPrime [a..2] ++ primesR 3 b
    -- only consider odd numbers
    | even a    = filter isPrime [a+1,a+3..b]
    | otherwise = filter isPrime [a,a+2..b]
