module ElevenToTwenty where

import OneToTen (encode)

-- Problem #11
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.

data EncodedElement a
    = Single a
    | Multiple Int a
    deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [EncodedElement a]
encodeModified = map (\(n, x) -> if (n == 1) then Single x else Multiple n x) . encode

-- Problem #12
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.

decodeElem :: EncodedElement a -> [a]
decodeElem (Single x) = [x]
decodeElem (Multiple n x) = replicate n x

decodeModified :: [EncodedElement a] -> [a]
decodeModified = concatMap decodeElem

-- Problem #13
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in
-- problem 9, but only count them. As in problem P11, simplify the result list
-- by replacing the singleton lists (1 X) by X.

encodeDirect :: (Eq a) => [a] -> [EncodedElement a]
encodeDirect [] = []
encodeDirect (x:xs)
    | n == 1    = Single x : encodeDirect xs
    | otherwise = Multiple n x : encodeDirect ys
    where
        (eq_x, ys) = span (==x) xs
        n = length eq_x + 1

-- Problem #14
-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- Problem #15
-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

-- Problem #16
-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = (take (n-1) xs) ++ (dropEvery (drop n xs) n)

-- Problem #17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- Problem #18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take end $ drop start xs
    where
        start = max 0 (i-1)
        end = j - start

-- Problem #19
-- Rotate a list N places to the left.

rotate :: [a] -> Int -> [a]
rotate xs n = (drop s xs) ++ (take s xs)
    where
        s = mod n $ length xs
