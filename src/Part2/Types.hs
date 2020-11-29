{-- DO NOT CHANGE! --}
-- | Types for Part2

module Part2.Types where

data ColorLetter
  = RED
  | GREEN
  | BLUE

data ColorPart
  = Red Int
  | Green Int
  | Blue Int
  deriving (Eq,Show,Read)

data Color = Color
  { red :: Int
  , green :: Int
  , blue :: Int
  } deriving (Eq,Show,Read)

data Tree a = Tree
  { left :: Maybe (Tree a)
  , root :: a
  , right :: Maybe (Tree a)
  } deriving (Eq,Show,Read)
