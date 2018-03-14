module Main where

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

main :: IO ()
main = do
  putStrLn "hello world"