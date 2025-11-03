{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module Lessons.Lesson05 where

import Data.Foldable
import Data.Monoid
import Data.Char (isAlpha)
import Data.List (isPrefixOf)
import Text.ParserCombinators.ReadP (get)
import qualified Control.Applicative as control

-- --do notation
-- we need to execute all lines in order to return 
-- do 
-- a<- 5
-- b<- 6
-- return 42

--monads: IO STM Maybe Either a b[] 

-- >>> fl
-- takes each element from the list and applies length function to it
-- [5,4]

fl :: [Integer]
fl = fmap length ["labas", "medi"]

-- >>> fm
-- Nothing


fm :: Maybe Integer
fm = fmap (+1) Nothing

-- >>> fe
-- Right 42
fe :: Either String Integer
fe = fmap (+1) (Right 41)

-- >>> fe
-- Left 41
fe' :: Either Integer Integer
fe' = fmap (+1) (Left 41) -- does not apply function because it is Left



fIO :: IO String
fIO = fmap (\a -> a ++ "!") getLine -- IO String

-- applicative functors

-- minimal definition pure and (<*>)
-- pure and return are basically the same
-- takes pure value and makes it into a specified context
--team spirit of haskell community
-- return in monad and pure in applicative functor are the same

p :: [Int]
p = pure 5 -- [5]

-- import control.Applicative
-- applicative f => f a -> f (a -> b) -> f b
-- fmap f a => 

-- (<$>) is called infix fmap apply applicative value to applicative funciton
-- (<*>) is called infix applicative

am :: Maybe Integer
--am = (Just 5) (just 41)
am = (+) <$> (Just 5) <*> (Just 41) -- Just 46
-- we add 5 and 41 inside the Maybe context

am' :: Maybe Integer
am' = (\a b c -> a + b + c) <$> (Just 5) <*> (Just 41) <*> (Just 100) -- Just 146
-- if we have nothing in there the result will be nothing

al :: [Integer]
al = (+) <$> [1,2,3] <*> [1, 2] -- [2,3,4,3,4,5]

ml :: [Integer]
ml = do
    a <- [1,2,3]
    b <- [1,2]
    pure (a + b)


-- well make parser functor and applicative functor


newtype Parser a = Parser { -- more efyshent? to use newtype?
    runParser :: String -> Either String (a, String)
    } deriving Show

parseLetter :: Parser Char
parseLetter (h:t) = Parser $ \case
        [] -> Left "Empty input"
        (h:t) -> 
 if isAlpha h
    then Right (h, t)
    else Left ("A letter is expected, but got " ++ [h])

    --lets make it an instance of a functor

    instance Functor Parser where
        fmap :: (a -> b) -> Parser a -> Parser b
        fmap f functor =  parser  $ \input -> --calling its constructor
            case runParser functor input of
                Left e -> Left e
                Right (v, r) -> Right (f v, r)
                --instead of direct pattern matching we use runParser to extract the function inside the parser
-- >>> runParser (fmap toUpper parseLetter) "abc"
-- Right ('A',"bc")

-- >>> runParser (fmap (\c -> [c,c]) parseLetter) "abc"
-- Right ("aa","bc")

--let create an instance of applicative
instance Applicative Parser where
    pure :: a Parser a
    pure a = Parser $ \input -> Right (a, input)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    af <*> aa = Parser $ \input -> --parser which returns a function. HOW COOL IS THAT???
    Left e1 -> left e1
    Right (f, r1) -> case runParser aa r1 of
        Left e2 -> Left e2
        Right (a, r2) -> Right (f a, r2)

        -- >>> runParser threeLetters "abcde"
        -- works kinda like and2, and3 and so on
        threeLetters :; Parser String
        threeLetters = (\a b c -> [a, b, c]) <$> parseLetter <*> parseLetter <*> parseLetter


-- we need orElse
--we have a class alternative

-- <|> works like orElse 
instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Left "No parse" --a parser that fails always

    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = Parser $ \input ->
        case runParser p1 input of
            Right r1 -> Right r1
            Left e1 -> case runParser p2 input of 
                Right r2 -> Right r2
                Left e2 -> Left $ e1 ++ "; " ++ e2

-- some = many1
-- many = many

-- >>> runParser parseString "ada564"
 -- Right ("ada","564")

 -- >>> runParser parseNonEmptyString "awfw564"
 -- Right ("awfw","564")

 -- >>> runParser parseNonEmptyString "564"
    -- Left ["At least on value required"]

parseNonEmptyString :: Parser String
parseNonEmptyString = some parseLetter


--LAB3

-- applcation must be usefull nowww
-- reimplement with the new signature
-- just copy paste
-- need to keep state of the problem , sth that we store
-- STM 
-- must be single atomically block
-- must be persistant (to a file?)
-- to write to the file we need to serialize data
-- car 2 arrived. in file. once in 4 s we take the state and make it into commands. We use iser commands to recreate the state
-- sla?
-- 