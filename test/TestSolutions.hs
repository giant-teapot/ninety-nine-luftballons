module Main where

import Test.HUnit
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import OneToTen
import ElevenToTwenty

-- Problems 1 to 10

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

-- Problems 1 to 10

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
    ]

main :: IO ()
main = defaultMain $ hUnitTestToTests testCases