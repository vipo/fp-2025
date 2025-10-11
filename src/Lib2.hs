{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Lib2 (
    parseCommand,
    ToCliCommand(..),
    process
) where

import qualified Lib1
import Data.Char (isAlpha, isDigit, isSpace)

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- | Parser combinators and helpers

-- | Consumes a character if it satisfies the predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p [] = Left "Empty input"
satisfy p (h:t)
  | p h       = Right (h, t)
  | otherwise = Left ("Unexpected char: " ++ [h])

-- | Parses a single letter.
parseLetter :: Parser Char
parseLetter = satisfy isAlpha

-- | Parses a single digit.
parseDigit :: Parser Char
parseDigit = satisfy isDigit

-- | Parses zero or more occurrences.
many :: Parser a -> Parser [a]
many p = many' p []
  where
    many' p' acc = \input ->
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

-- | Parses one or more occurrences.
many1 :: Parser a -> Parser [a]
many1 p = \input ->
  case many p input of
    Left e -> Left e
    Right ([], _) -> Left "At least one value required"
    Right a -> Right a

-- | Maps a function over a parser's result.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = \input ->
  case p input of
    Left e -> Left e
    Right (v, r) -> Right (f v, r)

-- | Parses an integer.
parseInteger :: Parser Integer
parseInteger = pmap read $ many1 parseDigit

-- | Parses a string of letters.
parseString :: Parser String
parseString = many1 parseLetter

-- | Skips leading spaces.
skipSpaces :: Parser ()
skipSpaces [] = Right ((), [])
skipSpaces input@(h:t)
    | isSpace h = skipSpaces t
    | otherwise = Right ((), input)

-- | Parser combinators

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 = \input ->
  case p1 input of
    Left err -> Left $ "and2 failed on first parser: " ++ err
    Right (result1, rest1) ->
      case p2 rest1 of
        Left err -> Left $ "and2 failed on second parser: " ++ err
        Right (result2, rest2) -> Right ((result1, result2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 = \input ->
  case and2 p1 p2 input of
    Left err -> Left $ "and3 failed on first two parsers: " ++ err
    Right ((r1, r2), rest1) ->
      case p3 rest1 of
        Left err -> Left $ "and3 failed on third parser: " ++ err
        Right (r3, rest2) -> Right ((r1, r2, r3), rest2)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 p1 p2 p3 p4 = \input ->
  case and3 p1 p2 p3 input of
    Left err -> Left $ "and4 failed on first three parsers: " ++ err
    Right ((r1, r2, r3), rest1) ->
      case p4 rest1 of
        Left err -> Left $ "and4 failed on fourth parser: " ++ err
        Right (r4, rest2) -> Right ((r1, r2, r3, r4), rest2)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = \input ->
  case p1 input of
    Right result -> Right result
    Left _ -> p2 input

-- | Tokenization

-- | Parses a single token: either a parenthesis or a word.
parseToken :: Parser String
parseToken = orElse parseParentheses (many1 (satisfy (\c -> not (isSpace c) && c /= '(' && c /= ')')))

-- | Parses a parenthesis as a token.
parseParentheses :: Parser String
parseParentheses = pmap (:[]) (satisfy (\c -> c == '(' || c == ')'))

-- | Parses a token, skipping leading spaces.
parseTokenWithSpace :: Parser String
parseTokenWithSpace = pmap snd (and2 skipSpaces parseToken)

-- | Parses all tokens from input.
parseTokens :: Parser [String]
parseTokens = many parseTokenWithSpace

-- | Parses a category, supporting nested categories.
parseCategory :: [String] -> Either String (Lib1.Category, [String])
parseCategory tokens =
  case tokens of
    [] -> Left "Expected category, got nothing"
    ("(":rest) ->
      case parseCategory rest of
        Left err -> Left $ "Error parsing inner category: " ++ err
        Right (innerCat, (")":rest')) ->
          Right (innerCat, rest')
        Right (_, other) ->
          Left $ "Expected closing ), got: " ++ show other
    (name:"(":rest) ->
      case parseCategory rest of
        Left err -> Left $ "Error parsing nested category: " ++ err
        Right (innerCat, (")":rest')) ->
          Right (Lib1.NestedCategory name innerCat, rest')
        Right (_, other) ->
          Left $ "Expected closing ), got: " ++ show other
    (name:rest) ->
      Right (Lib1.SimpleCategory name, rest)

-- | Parses a command from input string.
parseCommand :: Parser Lib1.Command
parseCommand input =
  case parseTokens input of
    Left err -> Left $ "Tokenization failed: " ++ err
    Right (tokens, rest) -> parseCommandTokens tokens rest

-- | Matches tokens to Command constructors.
parseCommandTokens :: [String] -> String -> Either String (Lib1.Command, String)
parseCommandTokens tokens rest =
  case tokens of
    ["list", "books"] -> Right (Lib1.ListBooks, "")
    ("remove":"book":title:_) -> Right (Lib1.RemoveBook title, "")
    ("checkout":title:username:_) -> Right (Lib1.CheckoutBook title username, "")
    ["dump", "examples"] -> Right (Lib1.Dump Lib1.Examples, "")
    ("return":title:_) -> Right (Lib1.ReturnBook title, "")
    ("add":"book":title:author:categoryTokens) ->
      case parseCategory categoryTokens of
        Left err -> Left $ "Failed to parse category: " ++ err
        Right (cat, []) -> Right (Lib1.AddBook title author cat, "")
        Right (cat, leftover) -> Left $ "Unexpected tokens after category: " ++ show leftover
    [] -> Left "Empty command"
    _ -> Left $ "Unknown command: " ++ show tokens

-- | Converts a Command to its CLI string representation.
class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"
  toCliCommand (Lib1.AddBook title author cat) =
    "add book " ++ title ++ " " ++ author ++ " " ++ toCliCategory cat
    where
      toCliCategory (Lib1.SimpleCategory name) = name
      toCliCategory (Lib1.NestedCategory name inner) =
        name ++ "(" ++ toCliCategory inner ++ ")"
  toCliCommand (Lib1.RemoveBook title) = "remove book " ++ title
  toCliCommand Lib1.ListBooks = "list books"
  toCliCommand (Lib1.CheckoutBook book user) = "checkout " ++ book ++ " " ++ user
  toCliCommand (Lib1.ReturnBook book) = "return " ++ book
  toCliCommand _ = "Not implemented"  -- Catch-all at the END

-- | Processes a command for display/output.
process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

-- | Manual Eq instance for Command
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  (Lib1.Dump d1) == (Lib1.Dump d2) = d1 == d2
  (Lib1.AddBook t1 a1 c1) == (Lib1.AddBook t2 a2 c2) =
    t1 == t2 && a1 == a2 && c1 == c2
  (Lib1.RemoveBook t1) == (Lib1.RemoveBook t2) = t1 == t2
  Lib1.ListBooks == Lib1.ListBooks = True
  (Lib1.CheckoutBook b1 u1) == (Lib1.CheckoutBook b2 u2) =
    b1 == b2 && u1 == u2
  (Lib1.ReturnBook b1) == (Lib1.ReturnBook b2) = b1 == b2
  _ == _ = False