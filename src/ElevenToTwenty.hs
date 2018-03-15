module ElevenToTwenty where

import OneToTen (encode)

-- Problem #11
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.

data EncodedElement a
    = Single a
    | Multiple Int a
    deriving Show

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
