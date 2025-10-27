{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lessons.Lesson04 where

import Data.Char (isAlpha, isDigit)
import Data.List (isPrefixOf)
import Control.Concurrent (Chan)

-- | The 'add' function takes two integers and returns their sum.
-- It is a simple example of a named function definition.
import Control.Monad


add :: Int -> Int -> Int
add a b = a + b

-- | Anonymous functions (also called lambda functions) are functions without a name.
-- In Haskell, they are written using a backslash `\`, followed by their parameters.
--
-- The function below demonstrates how 'foldl' can be used with both named and anonymous functions.
-- 'foldl' stands for fold left, meaning it combines all elements of a list using a binary function.
--
-- >>> foldl add 0 [1,2,3,4,5]
-- 15
--
-- >>> foldl add 0 []
-- 0
--
-- >>> foldl add (-1) []
-- -1
--
-- >>> foldl (\a b -> a * b) 1 [1,2,3,4]
-- 24
sumOfInts :: [Int] -> Int
sumOfInts [] = 0
sumOfInts (h:t) = h + sumOfInts t

-- | In this example, 't' represents the tail of a list — that is, all elements except the first.
-- This function shows how to apply a given function 'f' to every element in the list.
-- 
-- It is essentially a reimplementation of Haskell’s built-in 'map' function.

-- >>> map length ["labas", "medi"]
-- [5,4]
mapping :: (a -> b) -> [a] -> [b]
mapping _ [] = []
mapping f (h:t) = f h : mapping f t

-- | The 'Either' type is used to represent computations that can return one of two possible results — typically an error (Left) or a success (Right).
--
-- This function safely performs integer division. If the divisor is zero, it returns an error message wrapped in 'Left'. Otherwise, it returns the result in 'Right'.
--
-- >>> safeDivision 1 0
-- Left "Division by zero"
--
-- >>> safeDivision 1 10
-- Right 0
safeDivision :: Integer -> Integer -> Either String Integer
safeDivision _ 0 = Left "Division by zero"
safeDivision a b = Right (a `div` b)

-- | The type 'Parser a' represents a function that takes a string and tries to extract a value of type 'a' from its beginning.
-- It returns either:
--   'Left' with an error message, or
--   'Right' with a tuple ('a', 'String') — the parsed value and the remaining input.
type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- | This parser tries to read a single alphabetic letter from the beginning of a string.
--
-- If the input is empty, it fails with an error message.
-- If the first character is a letter, it returns it along with the rest of the string.
-- Otherwise, it returns an error describing what went wrong.
--
-- The operator '$' allows us to avoid parentheses for function application.
--
-- >>> parseLetter "fksdjhfdjk"
-- Right ('f',"ksdjhfdjk")
--
-- >>> parseLetter ""
-- Left "A letter is expected but got empty input"
--
-- >>> parseLetter "3213fksdjhfdjk"
-- Left "A letter is expected, but got 3"
parseLetter :: Parser Char
parseLetter [] = Left "A letter is expected but got empty input"
parseLetter (h:t) =
  if isAlpha h
    then Right (h, t)
    else Left $ "A letter is expected, but got " ++ [h]

-- | This parser works similarly to 'parseLetter', but it checks for digits.
--
-- >>> parseDigit "ghfjkd"
-- Left "A digit is expected, but got g"
--
-- >>> parseDigit "55ghfjkd"
-- Right ('5',"5ghfjkd")
parseDigit :: Parser Char
parseDigit [] = Left "A digit is expected but got empty input"
parseDigit (h:t) =
  if isDigit h
    then Right (h, t)
    else Left $ "A digit is expected, but got " ++ [h]

-- | The 'many' parser runs another parser repeatedly on the input.
-- It succeeds even if the inner parser matches zero times.
-- 
-- The helper function 'many'' carries an accumulator 'acc' that collects results.
-- 
-- 'v' represents the value returned by the inner parser and 'r' represents the remaining input string after parsing.
many :: Parser a -> Parser [a]
many p = many' p []
  where
    many' p' acc = \input ->
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

-- | The 'many1' parser behaves like 'many', but it requires at least one successful parse.
-- If the input does not contain any valid elements, it fails.
--
-- >>> parseString ""
-- Left "At least on value required"
-- >>> parseString "afds"
-- Right ("afds","")
-- >>> parseString "afds5345"
-- Right ("afds","5345")
many1 :: Parser a -> Parser [a]
many1 p = \input ->
  case many p input of
    Left e -> Left e
    Right ([], _) -> Left "At least on value required"
    Right a -> Right a

-- | This parser combines everything we have built so far.
-- It parses one or more letters from the beginning of the input string.
-- It succeeds if at least one letter is found, and returns the collected string.
--
-- >>> parseString "afds"
-- Right ("afds","")
--
-- >>> parseString "afds5345"
-- Right ("afds","5345")
--
-- >>> parseString "afds 5345"
-- Right ("afds"," 5345")
parseString :: Parser String
parseString = many1 parseLetter

-- | The 'pmap' function allows us to transform the result produced by a parser without changing how that parser actually reads the input string.
--
-- A parser of type 'Parser a' returns either:
--   'Left' with an error message if parsing fails, or
--   'Right (v, r)' where 'v' is the parsed value and 'r' is the remaining input.
--
-- 'pmap' takes a transformation function @f :: a -> b@ and a parser @p :: Parser a@,
-- and produces a new parser @Parser b@ that behaves just like @p@ but applies @f@
-- to the successfully parsed value before returning it.
--
-- The overall structure of the result ('Left' or 'Right', plus the remaining input)
-- stays exactly the same — only the parsed value itself is modified.
--
-- For example, if @many1 parseDigit@ returns @Right ("123", "")@,
-- then @pmap read (many1 parseDigit)@ will return @Right (123, "")@.
--
-- This pattern is the same idea as 'fmap' for functors: map over the value
-- inside a context (in this case, the parsing context).
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = \input ->
  case p input of
    Left e -> Left e
    Right (v, r) -> Right (f v, r)


-- | This parser reads one or more digits from the input string and converts them into an 'Integer'.
-- It demonstrates how to combine parsing ('many1 parseDigit') with transformation ('pmap read').
--
-- >>> parseInteger "3423432gfhg"
-- Right (3423432,"gfhg")
parseInteger :: Parser Integer
parseInteger = pmap read $ many1 parseDigit
