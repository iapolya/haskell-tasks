{-- DO NOT CHANGE! --}
-- | Types for Part4

module Part4.Types where

newtype Parser a = Parser
  { runParser :: String -> [(String, a)] }

parse :: Parser a -> String -> Either String a
parse (Parser pf) s = case pf s of
  [] -> Left "Can't parse"
  ((rest, x) : _)
    | null rest -> Right x
    | otherwise -> Left $ "Leftover: " ++ rest

satisfyP :: (Char -> Bool) -> Parser Char
satisfyP p = Parser $ \s -> case s of
  [] -> []
  (c:cs)
    | p c -> [(cs, c)]
    | otherwise -> []

anyCharP, digitP, letterP :: Parser Char
anyCharP = satisfyP $ const True
digitP = satisfyP $ \c -> '0' <= c && c <= '9'
letterP = satisfyP $ \c -> 'a' <= c && c <= 'z' ||
                           'A' <= c && c <= 'Z'

newtype Foo r a = Foo
  { unFoo :: (a -> r) -> r }
