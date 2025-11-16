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

type ErrorMsg = String
type Input = String
type Parser a = ExceptT ErrorMsg (State Input) a

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

parseTwoLetters :: Parser String
parseTwoLetters = do
    a <- parseLetter
    b <- parseLetter
    pure [a, b]

parseTwoLetters' :: Parser String
parseTwoLetters' = (\a b -> [a, b]) <$> parseLetter <*> parseLetter


-- >>> :t runExceptT parseTwoLetters
-- runExceptT parseTwoLetters :: State Input (Either ErrorMsg String)

-- >>> :t runState (runExceptT parseTwoLetters)
-- runState (runExceptT parseTwoLetters) :: Input -> (Either ErrorMsg String, Input)

-- >>> parse parseTwoLetters ""
-- (Left "A letter is expected but got empty input","")
-- >>> parse parseTwoLetters "ds"
-- (Right "ds","")
-- >>> parse parseTwoLetters "545435"
-- (Left "A letter is expected, but got 5","545435")

parse :: Parser a -> Input -> (Either ErrorMsg a, Input)
parse p = runState (runExceptT p)

type Weird a = ExceptT Int (StateT String IO) a

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

weird' :: Weird Double
weird' = do
    liftIO $ putStrLn "Hello?"
    answer <- liftIO $ getLine
    lift $ put answer
    return 3.14

data SomeData = Foo String | Bar Integer deriving Show

-- >>> generate arbitrary :: IO SomeData
instance Arbitrary SomeData where
  arbitrary :: Gen SomeData
  arbitrary = oneof [fmap Foo arbitrary, fmap Bar arbitrary]
