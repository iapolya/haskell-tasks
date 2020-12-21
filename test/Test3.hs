module Test3 (tests3) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.Hedgehog as HH
import Test.Tasty.HUnit

import Part3

tests3 :: [TestTree]
tests3 =
  [ test18
  , test19
  , test20
  , test21
  , test22
  , test23
  , test24
  , test25
  , test26
  , test27
  , test28
  , test29
  , test30
  , test31
  , test32
  ]

test18 :: TestTree
test18 = testGroup "P18"
  [ testCase "prob18 17 == T" $ prob18 17 @?= True
  , testCase "prob18 1 == F" $ prob18 1 @?= False
  ]

test19 :: TestTree
test19 = testGroup "P19"
  [ testCase "prob19 19 == [(19,1)]" $ prob19 19 @?= [(19,1)]
  , testCase "prob19 1 == []" $ prob19 1 @?= []
  , testCase "prob19 2020 == [(2,2),(5,1),(101,1)]" $
    prob19 2020 @?= [(2,2),(5,1),(101,1)]
  ]

test20 :: TestTree
test20 = testGroup "P20"
  [ testCase "prob20 6 == T" $ prob20 6 @?= True
  , testCase "prob20 8 == F" $ prob20 8 @?= False
  , testCase "prob20 1 == F" $ prob20 1 @?= False
  ]

test21 :: TestTree
test21 = testGroup "P21"
  [ testCase "prob21 6 == [1,2,3,6]" $ prob21 6 @?= [1,2,3,6]
  , testCase "prob21 7 == [1,7]" $ prob21 7 @?= [1,7]
  , testCase "prob21 1 == [1]" $ prob21 1 @?= [1]
  ]

test22 :: TestTree
test22 = testGroup "P22"
  [ testCase "prob22 \"part iii izi\" == 6" $
    prob22 "parti iii izi" @?= 6
  , testCase "prob22 \"I have no letters\" == 1" $
    prob22 "I have no letters" @?= 0
  , testCase "prob22 \"no letters\" == 1 " $
    prob22 "no letters" @?= 0
  ]

test23 :: TestTree
test23 = testGroup "P23"
  [ testCase "prob23 \"1-4: hello world!\" == \"hell\"" $
    prob23 "1-4: hello world!" @?= Just "hell"
  , testCase "prob23 \"10-11:  2345678901\" == \"01\"" $
    prob23 "10-11:  2345678901" @?= Just "01"
  , testCase "prob23 \"100-120: 1\" == Nothing" $
    prob23 "100-120: 1" @?= Nothing
  , testCase "prob23 \"5-3: abcde\" == \"edc\"" $
    prob23 "5-3: abcde" @?= Just "edc"
  ]

test24 :: TestTree
test24 = testGroup "P24"
  [ testCase "prob24 1 == T" $ prob24 1 @?= True
  , testCase "prob24 2 == F" $ prob24 2 @?= False
  , testCase "prob24 6 == T" $ prob24 6 @?= True
  , testCase "prob24 15 == T" $ prob24 15 @?= True
  ]

test25 :: TestTree
test25 = testGroup "P25"
  [ testCase "prob25 9 == T" $ prob25 9 @?= True
  , testCase "prob25 10 == F" $ prob25 10 @?= False
  , testCase "prob25 101 == T" $ prob25 101 @?= True
  ]

test26 :: TestTree
test26 = testGroup "P26"
  [ testCase "prob26 1 2 == F" $ prob26 1 2 @?= False
  , testCase "prob26 6 6 == T" $ prob26 6 6 @?= True
  , testCase "prob26 220 284 == T" $ prob26 220 284 @?= True
  ]

test27 :: TestTree
test27 = testGroup "P27"
  [ testCase "prob27 5 [1,2,3] == (2,3)" $
    prob27 5 [1,2,3] @?= Just (2,3)
  , testCase "prob27 5 [2,1,2] == N" $
    prob27 5 [2,1,2] @?= Nothing
  , testCase "prob27 4 [2,1,2] == (2,2)" $
    prob27 4 [2,1,2] @?= Just (2,2)
  , testCase "prob27 4 [1,2,1] == N" $
    prob27 4 [1,2,1] @?= Nothing
  ]

test28 :: TestTree
test28 = testGroup "P28"
  [ testCase "prob28 5 [3,1,1,1,2] == (2,1,1,1)" $
    prob28 5 [3,1,1,1,2] @?= Just (2,1,1,1)
  , testCase "prob28 5 [3,2,1,1,5] == N" $
    prob28 5 [3,2,1,1,5] @?= Nothing
  ]

test29 :: TestTree
test29 = testGroup "P29"
  [ testCase "prob29 1 == 9" $ prob29 1 @?= 9
  ]

test30 :: TestTree
test30 = testGroup "P30"
  [ testCase "prob30 4 == 6" $ prob30 4 @?= 6
  , testCase "prob30 5 == 28" $ prob30 5 @?= 28
  ]

test31 :: TestTree
test31 = testGroup "P31"
  [ testCase "prob31 250 == 0" $ prob31 250 @?= 0
  , testCase "prob31 300 == 504" $ prob31 300 @?= 504
  ]

test32 :: TestTree
test32 = testGroup "P32"
  [ testCase "prob32 [2,3,5] 10 == [5+5, 5+3+2, 3+3+2+2, 2+2+2+2+2]" $
    prob32 [2,3,5] 10 @?= [[5,5],[5,3,2],[3,3,2,2],[2,2,2,2,2]]
  ]
