module Main where

import Test.HUnit
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Data.List (nub, subsequences)
import H99.Lists
import H99.Arithmetic

-- H99.Lists

testProblem1 = TestCase $ do
    assertEqual "myLast [1,2,3,4]" 4 $ myLast [1,2,3,4]
    assertEqual "myLast ['x','y','z']" 'z' $ myLast ['x','y','z']

testProblem2 = TestCase $ do
    assertEqual "myButLast [1,2,3,4]" 3 $ myButLast [1,2,3,4]
    assertEqual "myButLast ['a'..'z']" 'y' $ myButLast ['a'..'z']

testProblem3 = TestCase $ do
    assertEqual "elementAt [1,2,3] 2" 2 $ elementAt [1,2,3] 2
    assertEqual "elementAt \"haskell\" 5" 'e' $ elementAt "haskell" 5

testProblem4 = TestCase $ do
    assertEqual "myLength [123, 456, 789]" 3 $ myLength [123, 456, 789]
    assertEqual "myLength \"Hello, world!\"" 13 $ myLength "Hello, world!"

testProblem5 = TestCase $ do
    assertEqual "myReverse \"A man, a plan, a canal, panama!\""
                "!amanap ,lanac a ,nalp a ,nam A"
                $ myReverse "A man, a plan, a canal, panama!"
    assertEqual "myReverse [1,2,3,4]" [4,3,2,1] $ myReverse [1,2,3,4]

testProblem6 = TestCase $ do
    assertEqual "isPalindrome [1,2,3]" False $ isPalindrome [1,2,3]
    assertEqual "isPalindrome \"madamimadam\"" True $ isPalindrome "madamimadam"
    assertEqual "isPalindrome [1,2,4,8,16,8,4,2,1]" True
                $ isPalindrome [1,2,4,8,16,8,4,2,1]

testProblem7 = TestCase $ do
    assertEqual "flatten (Elem 5)" [5] $ flatten (Elem 5)
    assertEqual "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])"
                [1,2,3,4,5]
                $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    assertEqual "flatten (List [])" ([]::[Int]) $ flatten (List ([]::[NestedList Int]))

testProblem8 = TestCase $ do
    assertEqual "compress \"aaaabccaadeeee\"" "abcade" $ compress "aaaabccaadeeee"

testProblem9 = TestCase $ do
    assertEqual "pack \"aaaabccaadeeee\"" ["aaaa","b","cc","aa","d","eeee"]
                $ pack "aaaabccaadeeee"

testProblem10 = TestCase $ do
    assertEqual "encode \"aaaabccaadeeee\""
                [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
                $ encode "aaaabccaadeeee"

testProblem11 = TestCase $ do
    assertEqual "encodeModified \"aaaabccaadeeee\"" 
                [ Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a'
                , Single 'd', Multiple 4 'e' ]
                $ encodeModified "aaaabccaadeeee"

testProblem12 = TestCase $ do
    assertEqual "decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']"
                "aaaabccaadeeee"
                $ decodeModified [ Multiple 4 'a', Single 'b', Multiple 2 'c'
                                 , Multiple 2 'a', Single 'd', Multiple 4 'e' ]

testProblem13 = TestCase $ do
    assertEqual "encodeDirect \"aaaabccaadeeee\""
                [ Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a'
                , Single 'd', Multiple 4 'e' ]
                $ encodeDirect "aaaabccaadeeee"

testProblem14 = TestCase $ do
    assertEqual "dupli [1, 2, 3]" [1,1,2,2,3,3] $ dupli [1, 2, 3]

testProblem15 = TestCase $ do
    assertEqual "repli \"abc\" 3" "aaabbbccc" $ repli "abc" 3

testProblem16 = TestCase $ do
    assertEqual "dropEvery \"abcdefghik\" 3" "abdeghk" $ dropEvery "abcdefghik" 3

testProblem17 = TestCase $ do
    assertEqual "split \"abcdefghik\" 3" ("abc", "defghik") $ split "abcdefghik" 3

testProblem18 = TestCase $ do
    assertEqual "slice \"abcdefghik\" 3 7" "cdefg" $ slice "abcdefghik" 3 7

testProblem19 = TestCase $ do
    assertEqual "rotate \"abcdefgh\"" "defghabc" $ rotate "abcdefgh" 3
    assertEqual "rotate \"abcdefgh\" (-2)" "ghabcdef" $ rotate "abcdefgh" (-2)

testProblem20 = TestCase $ do
    assertEqual "removeAt \"abcd\" 2" ('b',"acd") $ removeAt "abcd" 2

testProblem21 = TestCase $ do
    assertEqual "insertAt 'X' \"abcd\" 2" "aXbcd" $ insertAt 'X' "abcd" 2

testProblem22 = TestCase $ do
    assertEqual "range 4 9" [4,5,6,7,8,9] $ range 4 9

testProblem23 = TestCase $ do
    xs <- rnd_select [1..15] 3
    assertEqual "length $ rnd_select [1..15] 3" 3 $ length xs
    assertEqual "length . nub $ rnd_select [1..15] 3" 3 $ length $ nub xs
    assertEqual "all (flip elem [1..15]) $ rnd_select [1..15] 3" True $ all (flip elem [1..15]) xs
    -- Check that extracted elements are unique
    xs <- rnd_select [1..15] 15
    assertEqual "length . nub $ rnd_select [1..15] 15" 15 $ length $ nub xs

    xs'' <- rnd_select' [1..15] 3
    assertEqual "length $ rnd_select [1..15] 3" 3 $ length xs''
    assertEqual "all (flip elem [1..15]) $ rnd_select [1..15] 3" True $ all (flip elem [1..15]) xs''

testProblem24 = TestCase $ do
    xs <- diff_select 6 49
    assertEqual "length $ diff_select 6 49" 6 $ length xs
    assertEqual "all (flip elem [1..49]) $ diff_select 6 49" True $ all (flip elem [1..49]) xs
    xs' <- diff_select 5 1
    assertEqual "all (==1) $ diff_select 5 1" True $ all (==1) xs'

testProblem25 = TestCase $ do
    xs <- rnd_permu "abcdef"
    assertEqual "length $ rnd_permu \"abcdef\"" 6 $ length xs
    assertEqual "length . nub $ rnd_permu \"abcdef\"" 6 $ length $ nub xs
    assertEqual "all (flip elem \"abcdef\") $ rnd_permu \"abcdef\"" True $ all (flip elem "abcdef") xs

testProblem26 = TestCase $ do
    assertEqual "length $ combinations 3 \"abcdef\"" 20 $ length $ combinations 3 "abcdef"
    assertEqual "all (flip elem $ subsequences \"abcdef\") $ combinations 3 \"abcdef\"" True $ all (flip elem $ subsequences "abcdef") $ combinations 3 "abcdef"
    assertEqual "all ((==3) . length) $ combinations 3 \"abcdef\"" True $ all ((==3) . length) $ combinations 3 "abcdef"

testProblem27 = TestCase $ do
    assertEqual "length $ group [2,3,4] input" 1260 $ length $ group [2,3,4] input
    assertEqual "length $ group [2,2,5] input" 756 $ length $ group [2,2,5] input
    where
        input = ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]

testProblem28 = TestCase $ do
    assertEqual "map length input" (map length lsorted) $ map length $ lsort input
    assertEqual "frequencies . lfsort input" (frequencies lfsorted) $ frequencies . lfsort $ input
    where
        frequency xs x = length $ filter ((== length x) . length) xs
        frequencies xs = map (frequency xs) xs
        --
        input    = ["abc","de","fgh","de","ijkl","mn","o"]
        lsorted  = ["o","de","de","mn","abc","fgh","ijkl"]
        lfsorted = ["ijkl","o","abc","fgh","de","de","mn"]

-- H99.Arithmetic

testProblem31 = TestCase $ do
    assertEqual "filter isPrime [1..100]"
        [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67
        , 71, 73, 79, 83, 89, 97] $ filter isPrime [1..100]

testProblem32 = TestCase $ do
    assertEqual "gcd 36 63" 9 $ myGcd 36 63
    assertEqual "gcd -3 -6" 3 $ myGcd (-3) (-6)
    assertEqual "gcd -3 6" 3 $ myGcd (-3) 6

testProblem33 = TestCase $ do
    assertEqual "coprime 35 64" True $ coprime 35 64

testProblem34 = TestCase $ do
    assertEqual "totient 0" 0 $ totient 0
    assertEqual "totient 1" 1 $ totient 1
    assertEqual "totient 10" 4 $ totient 10
    assertEqual "totient <$> [40..42]" [16, 40, 12] $ totient <$> [40..42]

testProblem35 = TestCase $ do
    assertEqual "primeFactors 0" [] $ primeFactors 0
    assertEqual "primeFactors 1" [] $ primeFactors 1
    assertEqual "primeFactors 3" [3] $ primeFactors 3
    assertEqual "primeFactors 315" [3,3,5,7] $ primeFactors 315

testProblem36 = TestCase $ do
    assertEqual "primeFactorsMult 0" [] $ primeFactorsMult 0
    assertEqual "primeFactorsMult 1" [] $ primeFactorsMult 1
    assertEqual "primeFactorsMult 315" [(3,2),(5,1),(7,1)] $ primeFactorsMult 315

testProblem37 = TestCase $ do
    assertEqual "totient' 0" 0 $ totient' 0
    assertEqual "totient' 1" 1 $ totient' 1
    assertEqual "totient' 10" 4 $ totient' 10
    assertEqual "totient' <$> [40..42]" [16, 40, 12] $ totient' <$> [40..42]
    assertEqual "totient' <$> [0..100]" (totient <$> [0..100]) $ (totient' <$> [0..100])

testProblem39 = TestCase $ do
    assertEqual "primesR 10 20" [11,13,17,19] $ primesR 10 20
    assertEqual "primesR 1 7" [2,3,5,7] $ primesR 1 7
    assertEqual "primesR 2 7" [2,3,5,7] $ primesR 2 7

testProblem40 = TestCase $ do
    assertEqual "goldbach 28" (5,23) $ goldbach 28
    assertEqual "goldbach 4" (2,2) $ goldbach 4

-- Global test list

testCases = TestList
    [ TestLabel "Problem #1 (myReverse)" testProblem1
    , TestLabel "Problem #2 (myButLast)" testProblem2
    , TestLabel "Problem #3 (elementAt)" testProblem3
    , TestLabel "Problem #4 (myLength)" testProblem4
    , TestLabel "Problem #5 (myReverse)" testProblem5
    , TestLabel "Problem #6 (isPalindrome)" testProblem6
    , TestLabel "Problem #7 (flatten)" testProblem7
    , TestLabel "Problem #8 (compress)" testProblem8
    , TestLabel "Problem #9 (pack)" testProblem9
    , TestLabel "Problem #10 (encode)" testProblem10

    , TestLabel "Problem #11 (encodeModified)" testProblem11
    , TestLabel "Problem #12 (decodeModified)" testProblem12
    , TestLabel "Problem #13 (encodeDirect)" testProblem13
    , TestLabel "Problem #14 (dupli)" testProblem14
    , TestLabel "Problem #15 (repli)" testProblem15
    , TestLabel "Problem #16 (dropEvery)" testProblem16
    , TestLabel "Problem #17 (split)" testProblem17
    , TestLabel "Problem #18 (slice)" testProblem18
    , TestLabel "Problem #19 (rotate)" testProblem19
    , TestLabel "Problem #20 (removeAt)" testProblem20

    , TestLabel "Problem #21 (insertAt)" testProblem21
    , TestLabel "Problem #22 (range)" testProblem22
    , TestLabel "Problem #23 (rnd_select)" testProblem23
    , TestLabel "Problem #24 (diff_select)" testProblem24
    , TestLabel "Problem #25 (rnd_permu)" testProblem25
    , TestLabel "Problem #26 (combinations)" testProblem26
    , TestLabel "Problem #27 (group)" testProblem27
    , TestLabel "Problem #28 (lsort, lfsort)" testProblem28
    
    , TestLabel "Problem #31 (isPrime)" testProblem31
    , TestLabel "Problem #32 (myGcd)" testProblem32
    , TestLabel "Problem #33 (comprime)" testProblem33
    , TestLabel "Problem #34 (totient)" testProblem34
    , TestLabel "Problem #35 (primeFactors)" testProblem35
    , TestLabel "Problem #36 (primeFactorsMult)" testProblem36
    , TestLabel "Problem #37 (totient')" testProblem37
    , TestLabel "Problem #39 (primesR)" testProblem39
    , TestLabel "Problem #40 (goldbach)" testProblem40
    ]

main :: IO ()
main = defaultMain $ hUnitTestToTests testCases
