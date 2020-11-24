module Test1 (tests1) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.Hedgehog as HH
import Test.Tasty.HUnit

import Part1

tests1 :: [TestTree]
tests1 =
  [ test1
  , test2
  , test3
  , test4
  , test5
  ]

test1 :: TestTree
test1 = testGroup "P01"
  [ testCase "prob1 3 == 132" $ prob1 3 @?= 132
  , testCase "prob1 30000 == 24586" $ prob1 30000 @?= 24586
  ]

test2 :: TestTree
test2 = testGroup "P02"
  [ testCase "prob2 3 == 10" $ prob2 3 @?= 10
  , testCase "prob2 10 == 5" $ prob2 10 @?= 5
  ]

test3 :: TestTree
test3 = testGroup "P03"
  [ testCase "prob3 (-1) 5 == 4"  $ prob3 (\x -> x-1) 5 @?= 4
  , testCase "prob3 prob2 3 == 7" $ prob3 prob2 3 @?= 7
  ]

test4 :: TestTree
test4 = testGroup "P04"
  [ testCase "prob4 0 == 1" $ prob4 0 @?= 1
  , testCase "prob4 1 == 1" $ prob4 1 @?= 1
  , testCase "prob4 7 == 21"$ prob4 7 @?= 21
  , testCase "prob4 (-3) == -1" $ prob4 (-3) @?= -1
  , testCase "prob4 (-4) == 2"  $ prob4 (-4) @?= 2
  ]

test5 :: TestTree
test5 = testGroup "P05"
  [ testCase "prob5 98 10 == T" $ prob5 98 10 @?= True
  , testCase "prob5 77 10 == F" $ prob5 77 10 @?= False
  , testCase "prob5 13 13 == F" $ prob5 13 13 @?= False
  ]
