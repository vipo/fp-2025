-- | Notes taken by Lukas Zujevas
--
-- This lesson introduces a simple parser built using monad transformers.
-- The goal is to combine:
--
-- * 'State String' for input consumption
-- * 'Either String' for error reporting
--
-- via the 'EitherT' transformer.
--
-- This demonstrates how multiple effects can be composed into a single monad.

{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson15 where
import qualified Data.List as L

import Data.Char (isAlpha)

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (State, get, put, runState)

-- | A monad transformer that adds error handling to an existing monad
--
-- @EitherT e m a@ represents a computation in monad @m@ that can either:
--
-- * succeed with a value of type @a@
-- * fail with an error of type @e@
--
-- Internally, this is just a wrapper around @m (Either e a)@
newtype EitherT e m a = EitherT {
  runEitherT :: m (Either e a)
}

-- | Functor instance propagates errors and applies functions only to successful values.
instance Monad m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f ta = EitherT $ do
    a <- runEitherT ta
    case a of
      Left e -> return $ Left e
      Right r -> return $ Right $ f r

-- | Applicative instance executes computations one by one and stops at the first error encountered.
instance Monad m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ return $ Right a
  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  tf <*> ta = EitherT $ do
    f <- runEitherT tf
    case f of
      Left e1 -> return $ Left e1
      Right r1 -> do
        a <- runEitherT ta
        case a of
          Left e2 -> return $ Left e2
          Right r2 -> return $ Right (r1 r2)

-- | Monad instance enables sequencing computations using @do@-notation. Errors short-circuit the computation.
instance Monad m => Monad (EitherT e m) where
  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  m >>= k = EitherT $ do
    a <- runEitherT m
    case a of
      Left e -> return $ Left e
      Right r -> runEitherT (k r)

-- | A simple parser type.
--
-- * 'State String' stores the remaining input
-- * 'Either String' represents parsing failure
--
-- Running a parser produces a result and the leftover input.
type Parser a = EitherT String (State String) a

-- | Immediately fail with an error message.
--
-- The parser state is not modified.
throwE :: String -> Parser a
throwE msg = EitherT $ return $ Left msg

-- | Allows lifting actions from the inner monad into 'EitherT'.
--
-- Here used to lift 'State' operations.
instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift ma = EitherT $ fmap Right ma

-- | Parse a single alphabetic character.
--
-- Rules:
--
-- * If input is empty, parsing fails.
-- * If the first character is alphabetic, it is consumed and returned.
-- * Otherwise, parsing fails without consuming input.
--
-- Examples:
--
-- >>> :t runEitherT parseLetter
-- runEitherT parseLetter :: State String (Either String Char)
-- >>> :t runState (runEitherT parseLetter)
-- runState (runEitherT parseLetter) :: String -> (Either String Char, String)
-- >>> runState (runEitherT parseLetter) ""
-- (Left "A letter is expected but got empty input","")
-- >>> runState (runEitherT parseLetter) "13123"
-- (Left "A letter is expected, but got 1","13123")
-- >>> runState (runEitherT parseLetter) "abba"
-- (Right 'a',"bba")
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

-- | Parse two letters sequentially using monadic composition.
--
-- Example:
--
-- >>> runState (runEitherT twoLettersM) "abba"
-- (Right ('a','b'),"ba")
twoLettersM :: Parser (Char, Char)
twoLettersM = do
  a <- parseLetter
  b <- parseLetter
  return (a, b)

-- | Parse two letters using Applicative composition.
--
-- Equivalent to 'twoLettersM', but expressed without explicit binding.
--
-- Example:
--
-- >>> runState (runEitherT twoLettersA) "abba"
-- (Right ('a','b'),"ba")
twoLettersA :: Parser (Char, Char)
twoLettersA = (,) <$> parseLetter <*> parseLetter

