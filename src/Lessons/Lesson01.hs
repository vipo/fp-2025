-- | Notes taken by Ugnė Pacevičiūtė 
--
-- Module is a unit of compilation. A module can export values, functions,..
-- so they become accessible from other modules.
--
-- Core thing to remember: all values (not variables) are immutable!

module Lessons.Lesson01 (i, ii, c, s, b, f, add, il, cl) where

-- | First let's declare some values. 
-- In Integer is unbounded (or endless) integer value which
-- does not depend on your computer's architecture so never overflows.
-- The first line is a signature (`::` is a delimiter between a name and a type).
-- The second line is a body.
--
-- >>> i + i == 84
-- True
i :: Integer
i = 42

-- | An Int represents an integer which size respects your computer's architecture.
-- Might overflow.
ii :: Int 
ii = 43

-- | A single character
c :: Char
c = 'a'

-- | A String (technically, a list of Chars)
s :: String
s = "labas"

-- | A Boolean, might be `True` or `False`
b :: Bool
b = True

-- | So, let's assume we know how to declare values, but what about functions?
-- Functions and values are declared in a similar way, here we have a function
-- which takes an Integer as a parameter and returns a Bool.
--
-- Values are functions which do not take any arguments!
--
-- >>> f 23
-- True
--
-- >>> f 19
-- False
f :: Integer -> Bool
f age = age >= 20

-- | This is a bit more sophisticated case: function takes two arguments.
--
-- >>> add 20 22
-- 42
--
-- But what is we pass less arguments than needed? You get a function as a result!
--
-- >>> :t add 20
-- add 20 :: Integer -> Integer
--
-- >>> :t add
-- add :: Integer -> Integer -> Integer
--
-- This technique is called [Currying](https://en.wikipedia.org/wiki/Currying)

add :: Integer -> Integer -> Integer
add a b = a + b

-- | Another built-in type: a list. It has a special syntax
il :: [Integer]
il = [42, 42, 42]

-- | And, as it was mentioned a bit earlier, a String is a list of Chars.
cl :: [Char]
cl = "labas"
