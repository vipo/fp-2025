{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# HLINT ignore "Redundant lambda" #-}

module Lib2
  ( parseCommand,
    ToCliCommand (..),
    process,
  )
where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (isPrefixOf)
import qualified Lib1

type ErrorMsg = String

type Parser a = String -> Either ErrorMsg (a, String)

-- Basic parser combinators

satisfy :: (Char -> Bool) -> Parser Char
satisfy p [] = Left "Empty input"
satisfy p (h : t)
  | p h = Right (h, t)
  | otherwise = Left ("Unexpected char: " ++ [h])

parseLetter :: Parser Char
parseLetter = satisfy isAlpha

parseDigit :: Parser Char
parseDigit = satisfy isDigit

many :: Parser a -> Parser [a]
many p = go []
  where
    go acc input = case p input of
      Left _ -> Right (acc, input)
      Right (v, r) -> go (acc ++ [v]) r

many1 :: Parser a -> Parser [a]
many1 p = \input -> case many p input of
  Left e -> Left e
  Right ([], _) -> Left "At least one value required"
  Right a -> Right a

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = \input -> case p input of
  Left e -> Left e
  Right (v, r) -> Right (f v, r)

parseInteger :: Parser Integer
parseInteger = pmap read $ many1 parseDigit

parseWord :: Parser String
parseWord = many1 parseLetter

parseSpaces :: Parser ()
parseSpaces [] = Right ((), [])
parseSpaces input@(h : t)
  | isSpace h = parseSpaces t
  | otherwise = Right ((), input)

parseSpaces1 :: Parser ()
parseSpaces1 [] = Left "Expected at least one space"
parseSpaces1 (h : t)
  | isSpace h = parseSpaces t
  | otherwise = Left "Expected at least one space"

parseOpenParen  :: Parser String
parseOpenParen  = keyword "("

parseCloseParen :: Parser String
parseCloseParen = keyword ")"


and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 = \input -> case p1 input of
  Left err -> Left err
  Right (r1, rest1) -> case p2 rest1 of
    Left err -> Left err
    Right (r2, rest2) -> Right ((r1, r2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser ((a, b), c)
and3 p1 p2 p3 = \input -> case and2 p1 p2 input of
  Left err -> Left err
  Right ((r1, r2), rest) -> case p3 rest of
    Left err -> Left err
    Right (r3, rest2) -> Right (((r1, r2), r3), rest2)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (((a, b), c), d)
and4 p1 p2 p3 p4 = \input -> case and3 p1 p2 p3 input of
  Left err -> Left err
  Right ((r1r2, r3), rest) -> case p4 rest of
    Left err -> Left err
    Right (r4, rest2) -> Right (((r1r2, r3), r4), rest2)

and7 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser ((((((a, b), c), d), e), f), g)
and7 p1 p2 p3 p4 p5 p6 p7 input =
  case and4 p1 p2 p3 p4 input of
    Left e -> Left e
    Right ((((r1, r2), r3), r4), rest1) ->
      case and3 p5 p6 p7 rest1 of
        Left e -> Left e
        Right (((r5, r6), r7), rest2) ->
          Right (((((((r1, r2), r3), r4), r5), r6), r7), rest2)


and9 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser i -> Parser ((((((((a, b), c), d), e), f), g), h), i)
and9 p1 p2 p3 p4 p5 p6 p7 p8 p9 = \input ->
  case and4 p1 p2 p3 p4 input of
    Left err -> Left err
    Right ((((r1, r2), r3), r4), rest1) ->
      case and4 p5 p6 p7 p8 rest1 of
        Left err -> Left err
        Right ((((r5, r6), r7), r8), rest2) ->
          case p9 rest2 of
            Left err -> Left err
            Right (r9, rest3) -> Right (((((((((r1, r2), r3), r4), r5), r6), r7), r8), r9), rest3)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = \input -> case p1 input of
  Right result -> Right result
  Left _ -> p2 input

-- Command parsers

parseCommand :: Parser Lib1.Command
parseCommand =
  parseDump
    `orElse` parseAddBook
    `orElse` parseRemoveBook
    `orElse` parseListBooks
    `orElse` parseCheckoutBook
    `orElse` parseReturnBook

keyword :: String -> Parser String
keyword kw input
  | kw `isPrefixOf` input = Right (kw, drop (length kw) input)
  | otherwise = Left ("Expected keyword: " ++ kw)

parseDumpable :: Parser Lib1.Dumpable
parseDumpable = pmap (const Lib1.Examples) $ keyword "examples"

parseDump :: Parser Lib1.Command
parseDump =
  pmap (\((_, _), d) -> Lib1.Dump d) $
    and3 (keyword "dump") parseSpaces1 parseDumpable

parseAddBook :: Parser Lib1.Command
parseAddBook =
  pmap makeAddBook $
    and9
      (keyword "add")
      parseSpaces1
      (keyword "book")
      parseSpaces1
      parseBookTitle
      parseSpaces1
      parseAuthor
      parseSpaces1
      parseCategory
  where
    makeAddBook ((((((((_, _), _), _), title), _), author), _), cat) =
      Lib1.AddBook title author cat

parseRemoveBook :: Parser Lib1.Command
parseRemoveBook =
  pmap makeRemoveBook $
    and4
      (keyword "remove")
      parseSpaces1
      (keyword "book")
      (and2 parseSpaces1 parseBookTitle)
  where
    makeRemoveBook (((_, _), _), (_, title)) = Lib1.RemoveBook title

parseListBooks :: Parser Lib1.Command
parseListBooks =
  pmap (\((_, _), _) -> Lib1.ListBooks) $
    and3
      (keyword "list")
      parseSpaces1
      (keyword "books")

parseCheckoutBook :: Parser Lib1.Command
parseCheckoutBook =
  pmap makeCheckout $
    and4
      (keyword "checkout")
      parseSpaces1
      parseBookTitle
      (and2 parseSpaces1 parseUsername)
  where
    makeCheckout (((_, _), book), (_, user)) = Lib1.CheckoutBook book user

parseReturnBook :: Parser Lib1.Command
parseReturnBook =
  pmap makeReturn $
    and3
      (keyword "return")
      parseSpaces1
      parseBookTitle
  where
    makeReturn ((_, _), book) = Lib1.ReturnBook book

parseCategory :: Parser Lib1.Category
parseCategory = parseNestedCategory `orElse` parseSimpleCategory

parseSimpleCategory :: Parser Lib1.Category
parseSimpleCategory = pmap Lib1.SimpleCategory parseWord

parseNestedCategory :: Parser Lib1.Category
parseNestedCategory =
  pmap buildNested $
    and7
      parseWord
      parseSpaces1
      parseOpenParen
      parseSpaces1
      parseCategory
      parseSpaces1
      parseCloseParen
  where
    buildNested ((((((name, _), _), _), cat), _), _) =
      Lib1.NestedCategory name cat


parseBookTitle :: Parser String
parseBookTitle = parseWord

parseAuthor :: Parser String
parseAuthor = parseWord

parseUsername :: Parser String
parseUsername = parseWord

-- CLI string conversion

class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand (Lib1.Dump Lib1.Examples) = "dump examples"
  toCliCommand (Lib1.AddBook title author cat) =
    "add book " ++ title ++ " " ++ author ++ " " ++ toCliCategory cat
    where
      toCliCategory (Lib1.SimpleCategory name) = name
      toCliCategory (Lib1.NestedCategory name inner) =
        name ++ " ( " ++ toCliCategory inner ++ " )"
  toCliCommand (Lib1.RemoveBook title) = "remove book " ++ title
  toCliCommand Lib1.ListBooks = "list books"
  toCliCommand (Lib1.CheckoutBook book user) = "checkout " ++ book ++ " " ++ user
  toCliCommand (Lib1.ReturnBook book) = "return " ++ book
  toCliCommand _ = "Not implemented"

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

instance Eq Lib1.Command where
  (Lib1.Dump d1) == (Lib1.Dump d2) = d1 == d2
  (Lib1.AddBook t1 a1 c1) == (Lib1.AddBook t2 a2 c2) =
    t1 == t2 && a1 == a2 && c1 == c2
  (Lib1.RemoveBook t1) == (Lib1.RemoveBook t2) = t1 == t2
  Lib1.ListBooks == Lib1.ListBooks = True
  (Lib1.CheckoutBook b1 u1) == (Lib1.CheckoutBook b2 u2) =
    b1 == b2 && u1 == u2
  (Lib1.ReturnBook b1) == (Lib1.ReturnBook b2) = b1 == b2
  _ == _ = False