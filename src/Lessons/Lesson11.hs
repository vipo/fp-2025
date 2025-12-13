{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lessons.Lesson11 (Expr(..), eval) where

import Control.Exception
import Control.DeepSeq

-- | Haskell has exceptions. 
-- The error function is the simplest way to throw exceptions.
-- error is a pure function and can be any type.

-- | Here, the error function works as an integer, therefore it can be returned.
foo :: Integer
foo = error "Oops"

-- | For exception cathcing you need the Control.Exception package.

-- | The try function needs an IO block.
c :: Exception e => IO (Either e Integer)
c = try (return (error "Ooops"))

-- | If the code is multithreaded, you can send exceptions to any thread.

-- | Haskell is considered a lazy programming language. In a lazy language computations are only made if absolutely necessary.

-- | If one of the items in the list is an error, the final result will be the error.
-- This is an example of a strict/eager function.
-- >>> eager
-- oj
eager :: [Integer]
eager = [1,2,3,4, error "oj"]

-- | The take x function takes the first x elements from the list and returns them.
-- This is an example of a lazy function.
-- >>> lazy
-- [1,2]
lazy :: [Integer]
lazy = take 2 [1,2,3,4, error "ooops"]

-- | length is also a lazy function, because it counts how many items are in a list but does not care about their type (even if some of them may be errors).
-- >>> lazy'
-- 1
lazy' :: Int
lazy' = length [error "ooops"]

-- | With take 1, if the first element is an error, then the result is the error.
-- >>> lazy''
-- ooops
lazy'' :: [Integer]
lazy'' = take 1 [error "ooops"]

-- | Even though the integer list is infinite, first 10 works fine, because of the laziness.
-- If you were to print out the whole list [1..], it would just keep going forever to infinity.
-- >>> first 10
-- [1,2,3,4,5,6,7,8,9,10]
first :: Int -> [Integer]
first n = take n [1..]

-- | Lists are constructed using :, therefore the first n elements can be checked one by one.

-- | Eager calculations - trying to compute all data at once and store everything in memory. It can be achieved using libraries with the keyword "Strict".

-- | seq function: 
-- seq :: a -> b -> b
-- Simplest explanation: the return value is bottom if a is bottom, otherwise it is b.

-- | Returns 5 and ensures that everything was calculated/checked.
-- >>> seq 4 5
-- 5

-- >>> seq (error "Aj") 5
-- Aj

-- | Returns 5, because seq removes the top layer of what is uncalculated.
-- >>> seq [error "Aj"] 5
-- 5

-- | Does all of the calculations, no matter the layer.
-- >>> deepseq ([error "Aj"]:: [Int]) 5
-- Aj

-- | DSL - domain specific language
-- In our DSL's we had differend commands - a dictionary of tasks which we can use within our language.
-- A classic example - calculator.

-- Initial
-- Language
data Expr = Lit Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving Show
-- | Expr (expression) can be either a literal (number) or an operation for two expressions (add, mul) or an operation for one expression (neg).
-- This is already a DSL and now we can write programs using it.

-- Program
prog1 :: Expr
prog1 = Neg (Add (Lit 5) (Add (Lit 4) (Lit 6)))

-- | This is used to define how the program should act with different expressions.
-- Evaluate
-- >>> eval prog1
-- -15
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Neg e) = - (eval e)

-- | This is used for printing different expressions to the console.
-- >>> print' prog1
-- "-(5 + 4 + 6)"
print' :: Expr -> String
print' (Lit i) = show i
print' (Add e1 e2) = (print' e1) ++ " + " ++ (print' e2)
print' (Mul e1 e2) = (print' e1) ++ " * " ++ (print' e2)
print' (Neg e) = "-(" ++ (print' e) ++ ")"

-- | If the grammar is not initial, it's either final or tagless final
-- Tagless - because it doesnt have tags (like lit, add and so on).
-- FInal - because it is the final function.
-- Classes are used to achieve this. Classes define what we can do with something and not how to construct a value.

-- Final / Tagless Final
-- grammar
class Expression repr where
  lit :: Integer -> repr
  add :: repr -> repr -> repr
-- For example, the lit function takes an integer and returns its representation.

-- | The interpretor for the grammar.
-- eval
instance Expression Integer where
  lit :: Integer -> Integer
  lit i = i
  add :: Integer -> Integer -> Integer
  add a b = a + b

-- | You don't need to cover every instance for printing, it all depends on what you need for your project/business.
-- lit = show, because lit a = show a is redundant since they both have the same signature.
-- pretty print
instance Expression String where
  lit :: Integer -> String
  lit = show
  add :: String -> String -> String
  add a b = a ++ " + " ++ b

-- A new class is used to add multiplication.
class ExpressionMul repr where
  mul :: repr -> repr -> repr

instance ExpressionMul Integer where
  mul :: Integer -> Integer -> Integer
  mul a b = a * b

-- | Recreating prog1, just without negation; later we add multiplication
-- >>> prog2
-- 90
prog2 :: Integer
prog2 = mul (lit 6) (add (lit 5) (add (lit 4) (lit 6)))

-- | Returns a string of the whole expression.
-- >>> prog3
-- "5 + 4 + 6"
prog3 :: String
prog3 = add (lit 5) (add (lit 4) (lit 6))

--- GADT
data GExpr a where
    GInt :: Integer -> GExpr Integer
    GBool :: Bool -> GExpr Bool
    GAdd :: GExpr Integer -> GExpr Integer -> GExpr Integer
    GEq :: Eq a => GExpr a -> GExpr a -> GExpr Bool
-- | This solves the issue with types by using multiple constructors.

-- | You can't add GItn and GBool since they are of different types.
-- >>> evalG (GInt 5)
-- 5
-- >>> evalG (GAdd (GInt 5) (GInt 6))
-- 11
-- >>> evalG (GEq (GInt 5) (GInt 6))
-- False
-- >>> evalG (GAdd (GInt 5) (GBool False))
-- Couldn't match type `Bool' with `Integer'
-- Expected: GExpr Integer
--   Actual: GExpr Bool
-- In the second argument of `GAdd', namely `(GBool False)'
-- In the first argument of `evalG', namely
--   `(GAdd (GInt 5) (GBool False))'
-- In the expression: evalG (GAdd (GInt 5) (GBool False))
evalG :: GExpr a -> a
evalG (GInt i) = i
evalG (GBool b) = b
evalG (GAdd e1 e2) = evalG e1 + evalG e2
evalG (GEq e1 e2) = evalG e1 == evalG e2
