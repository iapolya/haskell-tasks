{-- DO NOT CHANGE! --}
-- | Types for Part4

module Part4.Types where

newtype Parser a = Parser
  { runParser :: String -> [(String, a)] }

newtype Foo r a = Foo
  { unFoo :: (a -> r) -> r }
