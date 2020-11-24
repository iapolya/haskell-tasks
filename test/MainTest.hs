{- DO NOT CHANGE! -}

module Main (main) where

import Test1
import Test2
import Test3
import Test4
import Hidden1
import Hidden2
import Hidden3
import Hidden4

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.Hedgehog as HH
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = localOption (mkTimeout 1000000) $
  testGroup "Parts"
  [ part1
  , part2
  , part3
  , part4
  ]

part1 :: TestTree
part1 = testGroup "Functions & Calls" $ tests1 ++ hidden1

part2 :: TestTree
part2 = testGroup "Types & Pattern Matching" $ tests2 ++ hidden2

part3 :: TestTree
part3 = testGroup "Lists & Standard Functions" $ tests3 ++ hidden3

part4 :: TestTree
part4 = testGroup "Functors, Applicatives & Monads" $ tests4 ++ hidden4
