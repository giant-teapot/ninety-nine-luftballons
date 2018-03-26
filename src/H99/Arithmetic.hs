module H99.Arithmetic where

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
