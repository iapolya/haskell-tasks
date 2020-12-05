module Test4 (tests4) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.Hedgehog as HH
import Test.Tasty.HUnit

import Control.Applicative
import Data.Char

import Part4.Types
import Part4

tests4 :: [TestTree]
tests4 =
  [ test33
  , test34
  , test35
  , test36
  , test37
  , test38
  , test39
  , test40
  ]

test33 :: TestTree
test33 = testGroup "P33" $ let
  toDigit c = ord c - ord '0'
  in [ testCase "fmap id" $
       parse (id <$> digitP) "1" @?= Right '1'
     , testCase "fmap f . fmap g" $
       parse (toDigit <$> digitP) "1" @?= Right 1
     ]

test34 :: TestTree
test34 = testGroup "P34"
  [ testCase "<*>" $
    parse ((,) <$> digitP <*> digitP) "12" @?= Right ('1','2')
  , testCase "pure" $
    parse (pure 123) "12" @?= Left "Leftover: 12"
  ]

test35 :: TestTree
test35 = testGroup "P35"
  [ testCase "<|>" $
    parse (many digitP) "123" @?= Right "123"
  ]

test36 :: TestTree
test36 = testGroup "P36"
  [ testCase ">>= 1" $
    parse p "12" @?= Right '2'
  , testCase ">>= 2" $
    parse p "23" @?= Right '2'
  ]
  where p = do
          x <- anyCharP
          y <- anyCharP
          case x of
            '1' -> pure y
            '2' -> pure x

test37 :: TestTree
test37 = testGroup "P37" []

test38 :: TestTree
test38 = testGroup "P38" []

test39 :: TestTree
test39 = testGroup "P39" []

test40 :: TestTree
test40 = testGroup "P40"
  [ testCase "varName_1:=123" $
    parse prob40 "varName_1:=123" @?= Right ("varName_1", 123)
  , testCase "theVAR  :=  -3456" $
    parse prob40 "theVAR  :=  -3456" @?= Right ("theVAR", -3456)
  ]
