import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

certainty = 10

trim = take certainty

list1 @??= list2 = trim list1 @?= trim list2 

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1
    
    , testCase "head' works on infinite list" $
        head' [1..] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on empty list" $
        tail' ([] :: [Int]) @?= []

    , testCase "tail' works on infinite list" $
        tail' [1..] @??= [2..]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 3 elements from 10-element list" $
        take' 3 [1..10] @?= [1,2,3]

    , testCase "take' 3 takes all elements from 2-element list" $
        take' 3 [1,2] @?= [1,2]

    , testCase "take' takes 3 elements from infinite list" $
        take' 3 [1..] @?= [1,2,3]

    , testCase "take' 0 returns empty list" $
        take' 0 [1,2,3] @?= []

    , testCase "take' works on empty list" $
        take' 10 ([] :: [Int]) @?= []

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' drops 3 elements from 10-element list" $
        drop' 3 [1..10] @?= [4..10]

    , testCase "drop' 3 drops all elements from 2-element list" $
        drop' 3 [1,2] @?= []

    , testCase "drop' drops 3 elements from infinite list" $
        drop' 3 [1..] @??= [4..]

    , testCase "drop' works on empty list" $
        drop' 3 ([] :: [Int]) @?= []

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' selects only even numbers from the naturals" $
        filter' even [1..] @??= [2,4..]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works when the second list is infinite" $
        concat' [1,2,3] [4..] @??= [1..]

    , testCase "concat' works when the first list is infinite" $
        concat' [1..] [(-1),(-2)..] @??= [1..]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]

    , testCase "quickSort' handles duplicates correctly" $
        quickSort' [5,2,3,4,2,4,3,1,1,5] @?= [1,1,2,2,3,3,4,4,5,5]
    ]
