{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module Lessons.Lesson05 where

import Data.Foldable
import Data.Monoid
import Data.Char (isAlpha)
import Data.List (isPrefixOf)

-- >>> mappend [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

-- >>> mempty :: [Integer]
-- []

-- >>> map Sum [1,2,3]
-- [Sum {getSum = 1},Sum {getSum = 2},Sum {getSum = 3}]

-- >>> getSum $ fold $ map Sum [1,2,3]
-- 6

-- >>> getProduct $ fold $ map Product [1,2,3]
-- 6

newtype MySum = MySum {getSum :: Integer}

-- >>> fold $ map All [True, True]
-- All {getAll = True}

-- >>> fold $ map All [True, True, False]
-- All {getAll = False}

-- >>> fold $ map Any [True, True, False]
-- Any {getAny = True}

-- >>> fold $ map Any [False, False]
-- Any {getAny = False}


type ErrorMsg = [String]
type Parser e a = String -> Either e (a, String)

parseLetter :: Parser ErrorMsg Char
parseLetter [] = Left ["A letter is expected but got empty input"]
parseLetter (h:t) =
  if isAlpha h
    then Right (h, t)
    else Left ["A letter is expected, but got " ++ [h]]

-- >>> parseString ""
-- Left ["At least on value required"]
-- >>> parseString "afds"
-- Right ("afds","")
-- >>> parseString "afds5345"
-- Right ("afds","5345")
-- >>> parseString "afds 5345"
-- Right ("afds"," 5345")
parseString :: Parser ErrorMsg String
parseString = many1 parseLetter

many :: Parser e a -> Parser e [a]
many p = many' p []
  where
    many' p' acc = \input ->
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

many1 :: Parser ErrorMsg a -> Parser ErrorMsg [a]
many1 p = \input ->
  case many p input of
    Left e -> Left e
    Right ([], _) -> Left ["At least on value required"]
    Right a -> Right a

pmap :: (a -> b) -> Parser e a -> Parser e b
pmap f p = \input ->
  case p input of
    Left e -> Left e
    Right (v, r) -> Right (f v, r)

data Food = Pizza | Sushi | Custom String deriving Show

orElse :: Semigroup e => Parser e a -> Parser e a -> Parser e a
orElse p1 p2 = \input ->
    case p1 input of
        Right r1 -> Right r1
        Left e1 ->
            case p2 input of
                Right r2 -> Right r2
                Left e2 -> Left $ e1 <> e2

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

keyword :: String -> Parser ErrorMsg String
keyword prefix input =
    if prefix `isPrefixOf` input then
        Right (prefix, drop (length prefix) input)
    else Left [prefix ++ " is expected, got " ++ input]

ws :: Parser ErrorMsg [String]
ws = many1 (keyword " " `orElse` keyword "\t")

parsePizza :: Parser ErrorMsg Food
parsePizza = pmap (const Pizza) $ keyword "pizza"

parseSushi :: Parser ErrorMsg Food
parseSushi = pmap (const Sushi) $ keyword "sushi"

parseCustom :: Parser ErrorMsg Food
parseCustom = pmap (\(_, _, s) -> Custom s) $ and3 (keyword "custom") ws parseString

-- >>> parseFood "pizza fdsf"
-- Right (Pizza," fdsf")
-- >>> parseFood "sushi"
-- Right (Sushi," ")
-- >>> parseFood "custom buritto "
-- Right (Custom "buritto"," ")
-- >>> parseFood "customburitto "
-- Left ["pizza is expected, got customburitto ","sushi is expected, got customburitto ","At least on value required"]
-- >>> parseFood "custom   "
-- Left ["pizza is expected, got custom   ","sushi is expected, got custom   ","At least on value required"]
parseFood :: Parser ErrorMsg Food
-- orElse (orElse parsePizza parseSushi) parseCustom
parseFood = parsePizza `orElse` parseSushi `orElse` parseCustom
