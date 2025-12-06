-- | Notes taken by Deimantė Davidavičiūtė
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lessons.Lesson10 where

import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

import Control.Monad
import Data.Foldable

import Control.Applicative


import Test.QuickCheck (Arbitrary, arbitrary, quickCheckResult)
import Test.QuickCheck.Gen

import Data.Char

-- | Basic types for the parser.
-- 'Parser a' is an ExceptT over State:
-- - 'State Input' carries the remaining input string.
-- - 'ExceptT ErrorMsg' allows early failure with an error message.
type ErrorMsg = String
type Input = String
type Parser a = ExceptT ErrorMsg (State Input) a

-- | Parse a single alphabetic character.
-- Consumes one character from the input state or fails with a message.
--
-- >>> parse parseLetter "a1"
-- (Right 'a',"1")
-- >>> parse parseLetter "1a"
-- (Left "A letter is expected, but got 1","1a")
-- >>> parse parseLetter ""
-- (Left "A letter is expected but got empty input","")
parseLetter :: Parser Char
parseLetter = do 
    input <- lift get
    case input of
        [] -> throwE "A letter is expected but got empty input"
        (h:t) -> if isAlpha h
            then do
                lift (put t)
                return h
            else throwE $ "A letter is expected, but got " ++ [h]

-- | Parse two letters using 'do' notation.
--
-- >>> parse parseTwoLetters "ds"
-- (Right "ds","")
-- >>> parse parseTwoLetters "d5"
-- (Left "A letter is expected, but got 5","5")
parseTwoLetters :: Parser String
parseTwoLetters = do
    a <- parseLetter
    b <- parseLetter
    pure [a, b]

-- | Same as 'parseTwoLetters', but using Applicative style.
--
-- >>> parse parseTwoLetters' "ab"
-- (Right "ab","")
parseTwoLetters' :: Parser String
parseTwoLetters' = (\a b -> [a, b]) <$> parseLetter <*> parseLetter


-- | Running the parser:
--
-- >>> :t runExceptT parseTwoLetters
-- runExceptT parseTwoLetters :: State Input (Either ErrorMsg String)
--
-- >>> :t runState (runExceptT parseTwoLetters)
-- runState (runExceptT parseTwoLetters) :: Input -> (Either ErrorMsg String, Input)
--
-- >>> parse parseTwoLetters ""
-- (Left "A letter is expected but got empty input","")
-- >>> parse parseTwoLetters "ds"
-- (Right "ds","")
-- >>> parse parseTwoLetters "545435"
-- (Left "A letter is expected, but got 5","545435")

-- | Helper to run a 'Parser' on an input string.
parse :: Parser a -> Input -> (Either ErrorMsg a, Input)
parse p = runState (runExceptT p)

-- | A more complex monad stack: IO + State + Except.
-- 'Weird a' fails with an Int, keeps a String state, and can do IO.
type Weird a = ExceptT Int (StateT String IO) a

-- | Version using nested 'lift' calls.
-- Interacts with the console and stores the last answer in state.
--
-- >>> :t runStateT (runExceptT weird) "init"
-- runStateT (runExceptT weird) "init" :: IO (Either Int Double, String)
weird :: Weird Double
weird = do
    lift $ lift $ putStrLn "Hello?"
    answer <- lift $ lift $ getLine
    lift $ put answer
    return 3.14

-- >>> :t weird
-- weird :: Weird Double
-- >>> :t runExceptT weird
-- runExceptT weird :: StateT String IO (Either Int Double)
-- >>> :t runStateT (runExceptT weird) "fsd"
-- runStateT (runExceptT weird) "fsd" :: IO (Either Int Double, String)

-- | Same behavior, but with 'liftIO' for cleaner IO lifting.
weird' :: Weird Double
weird' = do
    liftIO $ putStrLn "Hello?"
    answer <- liftIO $ getLine
    lift $ put answer
    return 3.14

-- | A simple sum type used with QuickCheck.
data SomeData = Foo String | Bar Integer deriving Show

-- | QuickCheck generator instance.
-- Produces either 'Foo <random string>' or 'Bar <random integer>'.
--
-- >>> generate arbitrary :: IO SomeData
instance Arbitrary SomeData where
    arbitrary :: Gen SomeData
    arbitrary = oneof [fmap Foo arbitrary, fmap Bar arbitrary]
