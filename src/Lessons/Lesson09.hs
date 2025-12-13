{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lessons.Lesson09 where

import Lessons.Lesson08(Parser(..), threeLetters)

import Control.Applicative

import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

import Control.Monad
import Data.Foldable

-- | Take a list as a traversable, pass a list of Maybies.
-- >>> sequenceA [Just 42, Just 5]
-- Just [42,5]
-- | Returns a Maybe containing a list.

-- | If the passed list contains a Nothing, Nothing is returned.
-- >>> sequenceA [Just 42, Just 5, Nothing]
-- Nothing

-- | Works pretty much as list comprehension:
-- >>> sequenceA [[42], [13, 45]]
-- [[42,13],[42,45]]

-- | Returns empty list:
-- >>> sequenceA [[42], [13, 45], []]
-- []

-- >>> sequenceA [Right 43, Right 45]
-- Right [43,45]

-- | Because a Left value was passed, it also returns a Left value:
-- >>> sequenceA [Left 0, Right 43, Right 45]
-- Left 0

-- >>> sequenceA $ Right (Right 45)
-- Right (Right 45)

-- >>> sequenceA $ Right (Left 45)
-- Left 45
-- | Returns the inner value (Left 45)

-- | Swaps the traversable with applicative:
-- >>> sequenceA $ Just (Right 45)
-- Right (Just 45)

-- | Collects all the inputs:
-- >>> :t sequenceA [getLine, getLine, getLine]
-- sequenceA [getLine, getLine, getLine] :: IO [String]
-- | Inner values are Just monadic values, type is IO [String].
-- | Result is an IO list of pure values, which makes life easier.

-- >>> :t [getLine, getLine, getLine]
-- [getLine, getLine, getLine] :: [IO String]

queryAge :: String -> IO Integer
queryAge name = do
    putStrLn $ "What is your age, " ++ name ++ "?"
    read <$> getLine
-- | Takes pure value (name) and returns a monadic value.

-- | If a list of names is provided, it returns a list of numbers (type IO [Integer]):
-- >>> :t mapM queryAge ["VI", "A", "U"]
-- mapM queryAge ["VI", "A", "U"] :: IO [Integer]

-- >>> :t map queryAge ["VI", "A", "U"]
-- map queryAge ["VI", "A", "U"] :: [IO Integer]
-- | Return type is [IO Integer] (not IO [Integer]).

-- | Usually functions with _'s drop the final result and only run side-effects.

-- >>> :t mapM_ queryAge ["VI", "A", "U"]
-- mapM_ queryAge ["VI", "A", "U"] :: IO ()
-- | Running a bunch of computations when you don't really care about the result.

-- >>> :t forM_ ["VI", "A", "U"] queryAge
-- forM_ ["VI", "A", "U"] queryAge :: IO ()
-- | Closest thing Haskell has to loop (similar to 'foreach' in other languages).
-- | Usually used for running a computation over some collection.
-- | Works the same way as mapM_ but has swapped arguments.

-- >>> liftM2 (+) (Just 4) (Just 6)
-- Just 10
-- | The number next to M shows how many arguments the first function takes.
-- | Works as (<$>).

-- >>>  (+) <$> (Just 4) <*> (Just 6)
-- Just 10

-- >>> liftM3 (\a b c -> a + b - c) (Just 4) (Just 6) Nothing
-- Nothing
-- | Lifts a pure function into a monadic function.

-- >>> liftA3 (\a b c -> a + b - c) (Just 4) (Just 6) Nothing
-- Nothing
-- | Same result as before.

-- | Using our helpers in the context of parsers (making them useful):

-- >>> :t threeLetters
-- threeLetters :: Parser String

-- >>> :t sequenceA [threeLetters, threeLetters, threeLetters]
-- sequenceA [threeLetters, threeLetters, threeLetters] :: Parser [String]

-- >>> :t runParser (sequenceA [threeLetters, threeLetters, threeLetters])
-- runParser (sequenceA [threeLetters, threeLetters, threeLetters]) :: String -> Either String ([String], String)

-- >>> runParser (sequenceA [threeLetters, threeLetters, threeLetters]) "asdasdasd!"
-- Right (["asd","asd","asd"],"!")
-- | After the first parser completes its course, the second parser continues with the unparsed string.

-- | Pretty much every program is just a traversable.

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) ->
                case runParser (mf a) r1 of
                    Left e2 -> Left e2
                    Right (b, r2) -> Right (b, r2)
-- | Allows to write parser combinators in a monadic way.

-- >>> runParser threeThreeLetterWords "asdasdasd!"
-- Right (["asd","asd","asd"],"!")
threeThreeLetterWords:: Parser [String]
threeThreeLetterWords = do
    a <- threeLetters
    b <- threeLetters
    c <- threeLetters
    pure [a, b, c]

-- | State monad - it tries to pretend that Haskell is a normal programming language with a mutable state.

-- >>> runState stateful "initial"
-- (7,"I am a new state")
stateful :: State String Int
stateful = do
    value <- get
    put "I am a new state"
    pure $ length value
-- | State has two params:
-- | - String: type of state;
-- | - Int: type of computation result.
-- | get - gets global state, put - changes global state.
-- | In this case, global is super local.

-- >>> runState combined "initial"
-- ((7,16),"I am a new state")
combined :: State String (Int, Int)
combined = do
    a <- stateful
    b <- stateful
    pure (a, b)
-- | 7 is the old state length, 16 is the new one.

-- >>> runState combined' "initial"
-- ((7,16),"I am a new state")
combined' :: State String (Int, Int)
combined' = (,) <$>  stateful <*> stateful

-- >>> runState combined'' "initial"
-- ((7,16),"I am a new state")
combined'' :: State String (Int, Int)
combined'' = liftA2 (,) stateful stateful
