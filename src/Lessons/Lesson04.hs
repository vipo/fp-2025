{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lessons.Lesson04 where

import Data.Char(isAlpha, isDigit)
import Data.List(isPrefixOf)
import Control.Concurrent (Chan)


add :: Int -> Int -> Int
add a b = a + b

-- >>> foldl add 0 [1,2,3,4,5]
-- 15
-- >>> foldl add 0 []
-- 0
-- >>> foldl add (-1) []
-- -1
-- >>> foldl (\a b -> a * b) 1 [1,2,3,4]
-- 24
sumOfInts :: [Int] -> Int
sumOfInts [] = 0
sumOfInts (h:t) = h + sumOfInts t


-- >>> map length ["labas", "medi"]
-- [5,4]

mapping :: (a -> b) -> [a] -> [b]
mapping _ [] = []
mapping f (h:t) = f h : mapping f t

-- >>> safeDivision 1 0
-- Left "Division by zero"
-- >>> safeDivision 1 10
-- Right 0
safeDivision :: Integer -> Integer -> Either String Integer
safeDivision _ 0 = Left "Division by zero"
safeDivision a b = Right (a `div` b)

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- >>> parseLetter "fksdjhfdjk"
-- Right ('f',"ksdjhfdjk")
-- >>> parseLetter ""
-- Left "A letter is expected but got empty input"
-- >>> parseLetter "3213fksdjhfdjk"
-- Left "A letter is expected, but got 3"
parseLetter :: Parser Char
parseLetter [] = Left "A letter is expected but got empty input"
parseLetter (h:t) =
  if isAlpha h
    then Right (h, t)
    else Left $ "A letter is expected, but got " ++ [h]

-- >>> parseDigit "ghfjkd"
-- Left "A digit is expected, but got g"
-- >>> parseDigit "55ghfjkd"
-- Right ('5',"5ghfjkd")
parseDigit :: Parser Char
parseDigit [] = Left "A digit is expected but got empty input"
parseDigit (h:t) = 
  if isDigit h
    then Right (h, t)
    else Left $ "A digit is expected, but got " ++ [h]

many :: Parser a -> Parser [a]
many p = many' p []
  where
    many' p' acc = \input ->
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

many1 :: Parser a -> Parser [a]
many1 p = \input ->
  case many p input of
    Left e -> Left e
    Right ([], _) -> Left "At least on value required"
    Right a -> Right a

-- >>> parseString ""
-- Left "At least on value required"
-- >>> parseString "afds"
-- Right ("afds","")
-- >>> parseString "afds5345"
-- Right ("afds","5345")
-- >>> parseString "afds 5345"
-- Right ("afds"," 5345")
parseString :: Parser String
parseString = many1 parseLetter

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = \input ->
  case p input of
    Left e -> Left e
    Right (v, r) -> Right (f v, r)

-- >>> parseInteger "3423432gfhg"
-- Right (3423432,"")
parseInteger :: Parser Integer
parseInteger = pmap read $ many1 parseDigit
