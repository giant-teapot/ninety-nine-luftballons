module H99.Lists where
-- This modules contains problems #1 to #28 on lists

import System.Random (getStdGen, randomRs)

-- Problem #1:
-- Find the last element of a list.

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast [] = error "empty list"

-- alternative:
myLast2 xs =
    let first = head xs
    in foldl (const id) first xs

-- Problem #2
-- Find the last but one element of a list.

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast (x:[]) = error "list of a single element"
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs

-- Problem #3
-- Find the K'th element of a list.
-- The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "index out of bounds"
elementAt (x:xs) n
    | n < 1     = error "index starts at 1"
    | n == 1    = x
    | otherwise = elementAt xs (n-1)

-- Problem #4
-- Find the number of elements of a list.

myLength :: (Foldable t) => t a -> Int
myLength = foldl (\acc _ -> acc + 1) 0

-- Problem #5
-- Reverse a list.

myReverse ::  [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem #6
-- Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- Problem #7
-- Flatten a nested list structure.

data NestedList a
    = Elem a
    | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap (flatten) xs

-- Problem #8
-- Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : dropWhile (==x) (compress xs)

-- Problem #9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- alternative
pack2 :: (Eq a) => [a] -> [[a]]
pack2 [] = []
pack2 (x:xs) = (x:ys) : pack2 zs
    where (ys, zs) = span (==x) xs

-- Problem #10
-- Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

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

-- Problem #20
-- Remove the K'th element from a list.
-- Index starts at 1.

removeAt :: [a] -> Int -> (a, [a])
removeAt xs n = (xs!!(n-1), (take (n-1) xs) ++ (drop n xs))

-- Problem #21
-- Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n-1) xs ++ x:drop (n-1) xs

-- Problem #22
-- Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range x y = [x..y]

-- Problem #23
-- Extract a given number of randomly selected elements from a list.

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    prng <- getStdGen
    return $ take n $ [ xs !! i | i <- randomRs (0, length xs - 1) prng ]

-- Problem #24
-- Lotto: Draw N different random numbers from the set 1..M.

diff_select :: Int -> Int -> IO [Int]
diff_select n limit = do
    prng <- getStdGen
    return $ take n $ [ x | x <- randomRs (1, limit) prng ]
