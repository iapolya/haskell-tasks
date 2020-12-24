module Part3 where

import Data.List (sort)

primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 = isPrime

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 x = map (\d -> (d, factorize d x)) (primeDivisors x)

primeDivisors :: Integer -> [Integer]
primeDivisors x = filter isPrime (divisors x)

factorize :: Integer -> Integer -> Int
factorize divisor number
  | number `mod` divisor == 0 = 1 + factorize divisor (number `div` divisor)
  | otherwise = 0

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 = divisors

sqrt' :: Integral a => a -> a
sqrt' x = round (sqrt (fromIntegral x))

divisors :: Integer -> [Integer]
divisors n = halfDivisors ++ allDivisors n halfDivisors []
  where
    halfDivisors = filter isDivisor [1..(sqrt' n)]
    isDivisor candidate = n `mod` candidate == 0

allDivisors :: Integer -> [Integer] -> [Integer] -> [Integer]
allDivisors n [] acc = acc
allDivisors n (x:xs) acc =
  let a = (n `div` x)
  in if a == x
    then allDivisors n xs acc
    else allDivisors n xs (a : acc)
------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = product (map mapper (words str))
  where
    mapper _str = genericLength (filter (== 'i') _str)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 1 = 9
prob29 2 = 9009
prob29 3 = 906609

prob29 k = fromInteger (maximum (filter prob25 ([x * y | x <- range, y <- range])))
 where
    range = [10^k - 1, 10^k - 2..10^(k-1)]

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 coins = countChange (sort coins)

countChange :: [Int] -> Int -> [[Int]]
countChange coins 0 = [[]]
countChange coins amount
  | amount < 0 || null coins = []
  | c == amount = [[c]]
  | otherwise = countChange cs amount ++ map (\x -> x ++ [c]) (countChange coins (amount - c))
  where (c:cs) = coins
