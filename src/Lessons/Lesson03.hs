{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson03 where

import Lessons.Lesson02 ( FEType(..) )

-- | We do not have loops in Haskell, because loops require a
-- mutable state (e.g. a counter), so we use recursion!
-- This function sums all integers in a given list.
-- If the list is empty, it returns 0 as the base case.
-- Otherwise, it takes the first element (the head) and adds it to the result of summing the rest of the list (the tail).
sumOfInts :: [Int] -> Int
sumOfInts [] = 0
sumOfInts (h:t) = h + sumOfInts t

-- | This function also recursively sums all integers in a given list.
-- But it has an advantage: instead of accumulating of intemediate results
-- (needed for + function) in stack, you can accumulate these values in a
-- dedicated argument, usually called "accumulator". This approach is called
-- "tail recursion" and modern compiles are smart enough to identify this
-- technic and optimize it (rewrite it with a machine code loop) so no stack
-- at all is used.
sumOfInts' :: [Int] -> Int
sumOfInts' l = sumOfInts'' l 0
  where
    sumOfInts'' :: [Int] -> Int -> Int
    sumOfInts'' [] acc     = acc
    sumOfInts'' (h:t) acc  = sumOfInts'' t (acc + h)

-- | In Haskell, a typeclass works similarly to an interface in Java.
--
-- The 'Command' type is an example of an Algebraic Data Type (ADT).
-- It represents a set of possible "commands" that a program might handle.
--
-- The left-hand side ('Command') defines the type name.
-- The right-hand side lists the constructors ('Dump' and 'Sum').
--
-- Each constructor defines a different *form* that a 'Command' value can take:
-- 'Dump' wraps a 'Dumpable' value.
-- 'Sum' wraps a list of integers that can be processed.
data Dumpable = Examples
data Command = Dump Dumpable | Sum [Int]

-- | This instance tells Haskell how to display values of type 'Dumpable'
-- when they are converted to a string using the 'show' function.
-- In this example, we have only one constructor, 'Examples', which
-- will simply be displayed as the word "examples". 
-- Normally, Haskell can automatically create a default implementation
-- when we write 'deriving Show', but here we define our own for learning purposes.
instance Show Dumpable where
  show :: Dumpable -> String
  show Examples = "examples"

-- | Here we define a custom 'Show' instance for the 'Command' type.
-- We want each command to be represented as a descriptive string.
-- If the command is 'Dump', it will print as "dump " followed by
-- whatever the inner 'Dumpable' value looks like.
-- If it is 'Sum is', it will print as "sum_of " followed by the list of numbers.
instance Show Command where
  show :: Command -> String
  show (Dump d) = "dump " ++ show d
  show (Sum is) = "sum_of " ++ show is

-- | The 'FuzzyAdd' typeclass represents a group of types that can be
-- combined using an “fuzzy” addition operation.
-- The operator (~+~) defines the behavior for this custom kind of addition.
class FuzzyAdd a where
  (~+~) :: a -> a -> a

-- | This instance defines how 'FuzzyAdd' works for integers.
-- Instead of performing a normal sum, it adds two numbersa
-- and then subtracts one. This is just an illustrative example
-- showing how we can redefine arithmetic operators in flexible ways.
instance FuzzyAdd Integer where
  (~+~) :: Integer -> Integer -> Integer
  (~+~) a b = a + b - 1

a1 :: Integer
a1 = 42

-- | FuzzyAdd use case.
--
-- >>> a1 ~+~ a2
-- 42
a2 :: Integer
a2 = 1


-- | Records are usual ADTs which allow to name fields.
-- The 'FireExtinguisher' type represents a simple data structure
-- describing a fire extinguisher. Each extinguisher has a capacity
-- and a type ('FEType'), which is imported from another module.
-- Records automatically provide accessors for record's fields.
data FireExtinguisher = FireExtinguisher
  { capacity :: Integer
  , feType   :: FEType
  } deriving Show

-- | This value represents a specific example of a fire extinguisher.
-- It has a capacity of 10 and is of type 'A'.
-- Accessor example:
--
-- >>> capacity fe
-- 10
fe :: FireExtinguisher
fe = FireExtinguisher 10 A

-- | This example shows how to use record update syntax in Haskell.
-- It creates a new extinguisher based on 'fe', but with the capacity
-- changed to 100, while keeping all other fields the same.
fe2 :: FireExtinguisher
fe2 = fe { capacity = 100 }

-- | This function extracts the 'capacity' field from a given
-- 'FireExtinguisher' record. It demonstrates simple pattern matching
-- on a record structure (just like usual ADTs)
cpcty :: FireExtinguisher -> Integer
cpcty (FireExtinguisher c _) = c

-- | The 'Bucket' data type defines another record-style structure.
-- It contains a single field, 'bucketCapacity', which stores how
-- much the bucket can hold. The 'deriving Show' clause again
-- provides a default string representation.
data Bucket = Bucket
  { bucketCapacity :: Integer
  } deriving Show

-- | What if we do not have a value to return? Other languages have nulls
-- for that. The 'safeHead' function retrieves the first element of a list
-- in a safe way. Normally, calling 'head' on an empty list causes
-- a runtime error. To prevent that, this function returns a 'Maybe' value:
-- 'Just a' if the list has a first element, or 'Nothing' if it is empty.
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (h:_) = Just h

-- | The 'safeHeadDefault' function is a practical extension of 'safeHead'.
-- It takes a list and a default value. If the list is empty, it returns
-- the default value; otherwise, it returns the first element of the list.
-- Internally, it uses pattern matching on the result of 'safeHead'
-- to decide which value to return.

safeHeadDefault :: [a] -> a -> a
safeHeadDefault l d =
  case safeHead l of
    Nothing -> d
    Just a  -> a
