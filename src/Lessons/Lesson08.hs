{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Lessons.Lesson08 (fio, Parser(..), threeLetters) where

import Data.Char
import Control.Applicative

-- | Monad is the most powerful class. 
-- Bind method (>>=) gives the illusion of sequential computation.

-- Functors ignore the order of execution.

-- | Minimal implementation of a functor:
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- It takes a pure function (which takes a and returns b), a functor of a, and returns a functor of b.


-- >>> fl
-- [5,4]
fl :: [Int]
fl = fmap length ["labas", "medi"]
-- The function was applied to every element of the list.

-- >>> fm
-- Nothing
fm :: Maybe Integer
fm = fmap (+1) Nothing
-- If the list is empty, the result is also an empty list.

-- >>> fe
-- Right 42
fe :: Either String Integer
fe = fmap (+1) $ Right 41

-- >>> fe'
-- Left 41
fe' :: Either Integer Integer
fe' = fmap (+1) $ Left 41
-- The left value is considered a "bad" value.

fio :: IO String
fio = fmap (\a -> a ++ "!") getLine
-- Takes an input and returns the value with "!"" in the end.
-- Returned type is IO String.

-- IO is a type which presents computations which originate in the external world.

-- Applicative functor is between functor and monad.

-- | pure is pretty much the same as return:
-- pure :: Applicative f => a -> f a
-- return :: Monad m => a -> m a

-- >>> p
-- [5]
p :: [Integer]
p = pure 5

-- | Signature <*>, sometimes called "spaceship".
-- (<*>) :: Applicative f => (a -> b) -> f a -> f b
-- It is similar to fmap, but instead of a Functor, it uses an Applicative.

-- Signature <$> is synonymous with fmap.

-- >>> am
-- Just 46
am :: Maybe Integer
am = (+) <$> (Just 5) <*> (Just 41)

-- >>> am'
-- Just 47
am' :: Maybe Integer
am' = (\a b c -> a + b + c) <$> (Just 5) <*> (Just 41) <*> (Just 1)

-- >>> am''
-- Nothing
am'' :: Maybe Integer
am'' = (\a b c -> a + b + c) <$> (Just 5) <*> Nothing <*> (Just 1)

-- >>> :t (\a b c -> a + b + c) <$> (Just 5) <*> Nothing
-- (\a b c -> a + b + c) <$> (Just 5) <*> Nothing :: Num a => Maybe (a -> a)

-- >>> :t (\a b c -> a + b + c) <$> (Just 5)
-- (\a b c -> a + b + c) <$> (Just 5) :: Num a => Maybe (a -> a -> a)

-- >>> :t fmap (\a b c -> a + b + c)
-- fmap (\a b c -> a + b + c) :: (Functor f, Num a) => f a -> f (a -> a -> a)

-- >>> al
-- [2,3,3,4,4,5]
al :: [Integer]
al = (+) <$> [1,2,3] <*> [1, 2]

-- >>> ml
-- [2,3,3,4,4,5]
ml :: [Integer]
ml = do
    a <- [1,2,3]
    b <- [1, 2]
    pure $ a + b

-- | al' and ml are basically the same thing:
-- In al, sources of values are completely independent, computed in parallel.
-- In ml, they depend on values that are above, line-by-line.


-- newtype is used if ADT has a single constructor.
newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

-- >>> runParser parseLetter "skfhsdk"
-- Right ('s',"kfhsdk")
parseLetter :: Parser Char
parseLetter = Parser $ \case 
    [] -> Left "A letter is expected but got empty input"
    (h:t) -> if isAlpha h
        then Right (h, t)
        else Left $ "A letter is expected, but got " ++ [h]
-- If a number is parsed, then it returns Left value.

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f functor = Parser $ \input ->
    case runParser functor input of
        Left e -> Left e
        Right (v, r) -> Right (f v, r)

-- >>> runParser (fmap (\c -> [c,c]) parseLetter) "labas"
-- Right ("ll","abas")

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  af <*> aa = Parser $ \input ->
    case runParser af input of
        Left e1 -> Left e1
        Right (f, r1) ->
            case runParser aa r1 of
                Left e2 -> Left e2
                Right (a, r2) -> Right (f a, r2)

-- >>> runParser threeLetters "labas"
-- Right ("lab","as")
threeLetters :: Parser String
threeLetters = (\a b c -> [a, b, c]) <$> parseLetter <*> parseLetter <*> parseLetter

-- | Signature <|>, sometimes called 'alternative'
-- >>> (Just 5) <|> (Just 6)
-- Just 5
-- >>> Nothing <|> (Just 6)
-- Just 6

-- >>> [] <|> [] <|> [1,2,3]
-- [1,2,3]

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left "No alternatives"
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
        Right r2 -> Right r2
        Left e1 ->
            case runParser p2 input of
                Right r2 -> Right r2
                Left e2 -> Left $ e1 ++ "; " ++ e2 
-- Can't be tested yet since we only have one parser!

-- >>> runParser parseString "4123"
-- Right ("","4123")
-- >>> runParser parseString "asd4123"
-- Right ("asd","4123")
parseString :: Parser String
parseString = many parseLetter

-- >>> runParser parseNonEmptyString "4123"
-- Left "A letter is expected, but got 4"
-- >>> runParser parseNonEmptyString "asd4123"
-- Right ("asd","4123")
parseNonEmptyString :: Parser String
parseNonEmptyString = some parseLetter
