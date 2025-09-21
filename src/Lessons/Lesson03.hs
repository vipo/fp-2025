{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE InstanceSigs #-}
module Lessons.Lesson03 where

import Lessons.Lesson02 ( FEType(..) )

sumOfInts :: [Int] -> Int
sumOfInts [] = 0
sumOfInts (h:t) = h + sumOfInts t

sumOfInts' :: [Int] -> Int
sumOfInts' l = sumOfInts'' l 0
    where
        sumOfInts'' :: [Int] -> Int -> Int
        sumOfInts'' [] acc = acc
        sumOfInts'' (h:t) acc = sumOfInts'' t (acc + h)

data Dumpable = Examples
data Command = Dump Dumpable | Sum [Int]

instance Show Dumpable where
  show :: Dumpable -> String
  show Examples = "examples"

instance Show Command where
  show :: Command -> String
  show (Dump d) = "dump " ++ show d
  show (Sum is) = "sum_of " ++ show is

class FuzzyAdd a where
    (~+~) :: a -> a -> a

a1 :: Integer
a1 = 42

a2 :: Integer
a2 = 1

instance FuzzyAdd Integer where
  (~+~) :: Integer -> Integer -> Integer
  (~+~) a b = a + b - 1

data FireExtinguisher = FireExtinguisher {
    capacity :: Integer,
    feType :: FEType
} deriving Show

fe :: FireExtinguisher
fe = FireExtinguisher 10 A

fe2 :: FireExtinguisher
fe2 = fe {capacity = 100}

cpcty :: FireExtinguisher -> Integer
cpcty (FireExtinguisher c _) = c

data Bucket = Bucket {
    bucketCapacity :: Integer
} deriving Show

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (h:_) = Just h

safeHeadDefault :: [a] -> a -> a
safeHeadDefault l d =
    case safeHead l of
        Nothing -> d
        Just a -> a
