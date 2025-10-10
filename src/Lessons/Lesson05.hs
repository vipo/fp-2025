{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module Lessons.Lesson05 where

import Data.Foldable
import Data.Monoid
import Data.Char (isAlpha)
import Data.List (isPrefixOf)
import qualified Control.Applicative as B


-- | A 'Monoid' is a typeclass that describes types which have:
--
--   * an **associative binary operation** (`mappend` or the `<>` operator)
--   * and a **neutral element** (`mempty`), also called “zero”.
--
-- The binary operation must satisfy associativity:
-- @(a <> b) <> c == a <> (b <> c)@
--
-- Monoids are powerful because they let you combine values of the same type
-- in a consistent and predictable way — for example, lists can be concatenated,
-- numbers can be summed, and boolean values can be combined with logical AND/OR.
--
-- >>> mappend [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
--
-- >>> mempty :: [Integer]
-- []
--
-- >>> map Sum [1,2,3]
-- [Sum {getSum = 1},Sum {getSum = 2},Sum {getSum = 3}]
--
-- >>> getSum $ fold $ map Sum [1,2,3]
-- 6
--
-- >>> getProduct $ fold $ map Product [1,2,3]
-- 6
--
-- |  The 'newtype' keyword defines a *lightweight wrapper* around an existing type.
-- It looks similar to a 'data' declaration but has an important difference:
-- there is **only one possible constructor** and **only one contained value**.
--
-- Because of this simplicity, the compiler can completely remove the wrapper at runtime,
-- meaning there is **no performance cost** — it behaves just like the original type
-- but is treated as a *distinct type* by the compiler.
--
-- In this example, 'MySum' wraps an 'Integer' value and can represent
-- a special-purpose numeric type — for instance, one that defines
-- how numbers should combine when added together in a monoid.
--
-- The record syntax '{ getSum :: Integer }' defines a helper function 'getSum' that extracts the inner value.
--
newtype MySum = MySum { getSum :: Integer }


-- | Folding combines all elements of a structure using a monoid.
-- However, 'fold' needs both:
--   1. The type to be 'Foldable', and
--   2. The elements themselves to form a 'Monoid'.
--
-- >>> fold $ map All [True, True]
-- All {getAll = True}
--
-- >>> fold $ map All [True, True, False]
-- All {getAll = False}
--
-- >>> fold $ map Any [True, True, False]
-- Any {getAny = True}
--
-- >>> fold $ map Any [False, False]
-- Any {getAny = False}

type ErrorMsg = [String]
type Parser e a = String -> Either e (a, String)

-- | This parser attempts to read a single alphabetic character from the start of the input.
-- If the input is empty, it fails with a descriptive error message.
-- If the first character is a letter, it succeeds and returns that letter with the remaining string.
-- If the first character is not a letter, it returns an error message explaining what was found.
parseLetter :: Parser ErrorMsg Char
parseLetter [] = Left ["A letter is expected but got empty input"]
parseLetter (h:t) =
  if isAlpha h
    then Right (h, t)
    else Left ["A letter is expected, but got " ++ [h]]


-- | The 'many' parser repeatedly applies another parser until it fails.
-- It succeeds even if the inner parser never succeeds, returning an empty list in that case.
--
-- Internally, the helper function 'many'' maintains an accumulator called 'acc',
-- which collects all successfully parsed values. Each time the parser 'p' succeeds,
-- the parsed value 'v' is appended to 'acc', and the remaining input 'r' is passed
-- into the next recursive call to continue parsing.
--
-- The variable 'v' represents one successfully parsed element,
-- while 'r' represents the unparsed remainder of the input.
--
-- The arrow symbol ('->') in the type signature means "a function that takes an input
-- of one type and returns an output of another type."
--
-- Initially, 'acc' is an empty list.
many :: Parser e a -> Parser e [a]
many p = many' p []
  where
    many' p' acc = \input ->
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r


-- | The 'many1' parser behaves like 'many', but it requires at least one successful parse.
-- If no values were parsed, it returns an error message.
--
-- >>> parseString ""
-- Left ["At least on value required"]
-- >>> parseString "afds"
-- Right ("afds","")
-- >>> parseString "afds5345"
-- Right ("afds","5345")
many1 :: Parser ErrorMsg a -> Parser ErrorMsg [a]
many1 p = \input ->
  case many p input of
    Left e -> Left e
    Right ([], _) -> Left ["At least on value required"]
    Right a -> Right a

-- | The 'pmap' function transforms the value returned by a parser without changing how the parser actually reads the input.
--
-- If the parser fails, the same failure is returned unchanged.
-- If it succeeds, the function 'f' is applied to the parsed value, and the result
-- is wrapped back into 'Right' together with the unmodified remaining input.

pmap :: (a -> b) -> Parser e a -> Parser e b
pmap f p = \input ->
  case p input of
    Left e -> Left e
    Right (v, r) -> Right (f v, r)

-- | The 'Food' data type represents several possible food items.
-- It is an example of an *algebraic data type (ADT)* that can have multiple constructors — each representing a different form of value.
-- 
-- * 'Pizza' and 'Sushi' are simple constructors with no extra data.
-- * 'Custom' takes a 'String' argument, allowing the user to specify a custom food name.
--
-- >>> Pizza
-- Pizza
-- >>> Custom "burger"
-- Custom "burger"
data Food = Pizza | Sushi | Custom String deriving Show


-- | The 'orElse' function is a *parser combinator* that represents a choice between two parsers.
-- It tries to run the first parser, and if it fails, it runs the second one.
--
-- Both parsers must produce errors of a type that is a 'Semigroup', meaning their error messages can be concatenated with the '<>' operator.
--
-- If both parsers fail, their error messages are merged into one combined list, providing clearer diagnostic output.
--
-- >>> (keyword "yes" `orElse` keyword "no") "maybe"
-- Left ["yes is expected, got maybe","no is expected, got maybe"]
orElse :: Semigroup e => Parser e a -> Parser e a -> Parser e a
orElse p1 p2 = \input ->
  case p1 input of
    Right r1 -> Right r1
    Left e1 ->
      case p2 input of
        Right r2 -> Right r2
        Left e2 -> Left $ e1 <> e2


-- | The 'and3' function runs three parsers one after another, in sequence.
-- Each parser consumes part of the input and passes the remaining string to the next one.
--
-- If all three succeed, their results are combined into a tuple '(a, b, c)'.
-- If any of them fails, the entire chain fails immediately with the first error.
--
-- The number (3) in the name simply indicates that it combines **three** parsers.
and3 :: Parser e a -> Parser e b -> Parser e c -> Parser e (a, b, c)
and3 p1 p2 p3 input =
  case p1 input of
    Left e1 -> Left e1
    Right (v1, r1) ->
      case p2 r1 of
        Left e2 -> Left e2
        Right (v2, r2) ->
          case p3 r2 of
            Left e3 -> Left e3
            Right (v3, r3) -> Right ((v1, v2, v3), r3)


-- | The 'keyword' parser checks whether the given prefix appears at the very beginning of the input string.
-- 
-- If the prefix matches, it returns that prefix and removes it from the input so that further parsers can continue processing the remaining text. 
-- Otherwise, it fails with a descriptive error message.
--
-- Internally, it uses two key functions from the standard library:
--
-- * 'isPrefixOf' (from "Data.List") — checks whether the first argument is a prefix of the second.
--
-- * 'drop' — removes a given number of characters
--   from the beginning of a list or string.
keyword :: String -> Parser ErrorMsg String
keyword prefix input =
  if prefix `isPrefixOf` input
    then Right (prefix, drop (length prefix) input)
    else Left [prefix ++ " is expected, got " ++ input]


-- | The 'ws' parser matches one or more whitespace elements.
-- It combines two 'keyword' parsers — one for spaces and one for tabs using 'orElse', allowing either to succeed.
-- The 'many1' combinator ensures at least one whitespace is present.
ws :: Parser ErrorMsg [String]
ws = many1 (keyword " " `orElse` keyword "\t")

-- | The expression '(const Pizza)' creates a function that ignores its input and always returns the 'Pizza' value. 
-- Using 'pmap', we apply this constant mapping to the keyword parser, meaning:
-- if the input starts with the word "pizza", it parses successfully
-- and produces the value 'Pizza'.
parsePizza :: Parser ErrorMsg Food
parsePizza = pmap (const Pizza) $ keyword "pizza"

-- | Similarly, this parser recognizes the keyword "sushi" and produces the 'Sushi' constructor.
parseSushi :: Parser ErrorMsg Food
parseSushi = pmap (const Sushi) $ keyword "sushi"

parseString :: Parser ErrorMsg String
parseString = many1 parseLetter

-- | The 'parseCustom' parser recognizes a custom food definition.
-- It expects the input to follow this pattern: "custom" <whitespace> <name>
--
-- The 'and3' combinator runs these three parsers in sequence and returns
-- all their results as a 3-tuple: (keywordResult, whitespaceResult, nameResult).
--
-- Finally, the lambda function '(\(_, _, s) -> Custom s)' takes that tuple,
-- ignores the first two values (since we only care about the food name),
-- and wraps the third value 's' inside the 'Custom' data constructor
-- to produce a value of type 'Food'.
--
parseCustom :: Parser ErrorMsg Food
parseCustom = pmap (\(_, _, s) -> Custom s) $ and3 (keyword "custom") ws parseString

-- | The 'parseFood' parser combines all previously defined food parsers.
-- It first tries to parse "pizza"; if that fails, it tries "sushi";
-- and if that also fails, it tries "custom".
--
-- The 'orElse' combinator is used to chain these parsers together, ensuring
-- that each one is attempted in order. If all of them fail, their error
-- messages are concatenated into a single list.
--
-- This demonstrates how multiple independent parsers can be composed into one higher-level parser that can recognize multiple patterns.
--
-- >>> parseFood "pizza fdsf"
-- Right (Pizza," fdsf")
--
-- >>> parseFood "sushi"
-- Right (Sushi," ")
--
-- >>> parseFood "custom buritto "
-- Right (Custom "buritto"," ")
--
-- >>> parseFood "customburitto "
-- Left ["pizza is expected, got customburitto ","sushi is expected, got customburitto ","At least on value required"]
--
-- >>> parseFood "custom   "
-- Left ["pizza is expected, got custom   ","sushi is expected, got custom   ","At least on value required"]
parseFood :: Parser ErrorMsg Food
parseFood = parsePizza `orElse` parseSushi `orElse` parseCustom

