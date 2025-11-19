{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lessons.Lesson11 () where

import Control.Exception
import Control.DeepSeq

foo :: Integer
foo = error "Oops"

c :: Exception e => IO (Either e Integer)
c = try (return (error "Ooops"))

-- >>> eager
-- oj
eager :: [Integer]
eager = [1,2,3,4, error "oj"]

-- >>> lazy
-- [1,2]
lazy :: [Integer]
lazy = take 2 [1,2,3,4, error "ooops"]

-- >>> lazy'
-- 1
lazy' :: Int
lazy' = length [error "ooops"]

-- >>> lazy''
-- ooops
lazy'' :: [Integer]
lazy'' = take 1 [error "ooops"]

-- >>> first 10
-- [1,2,3,4,5,6,7,8,9,10]
first :: Int -> [Integer]
first n = take n [1..]

-- >>> seq 4 5
-- 5

-- >>> seq (error "Aj") 5
-- Aj

-- >>> seq [error "Aj"] 5
-- 5

-- >>> deepseq ([error "Aj"]:: [Int]) 5
-- Aj

-- Initial
-- Language
data Expr = Lit Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving Show

-- Program
prog1 :: Expr
prog1 = Neg (Add (Lit 5) (Add (Lit 4) (Lit 6)))

-- Evaluate
-- >>> eval prog1
-- -15
eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Neg e) = - (eval e)

-- >>> print' prog1
-- "-(5 + 4 + 6)"
print' :: Expr -> String
print' (Lit i) = show i
print' (Add e1 e2) = (print' e1) ++ " + " ++ (print' e2)
print' (Mul e1 e2) = (print' e1) ++ " * " ++ (print' e2)
print' (Neg e) = "-(" ++ (print' e) ++ ")"

-- Final / Tagless Final
-- grammar
class Expression repr where
  lit :: Integer -> repr
  add :: repr -> repr -> repr

-- eval
instance Expression Integer where
  lit :: Integer -> Integer
  lit i = i
  add :: Integer -> Integer -> Integer
  add a b = a + b

-- pretty print
instance Expression String where
  lit :: Integer -> String
  lit = show
  add :: String -> String -> String
  add a b = a ++ " + " ++ b

class ExpressionMul repr where
  mul :: repr -> repr -> repr

instance ExpressionMul Integer where
  mul :: Integer -> Integer -> Integer
  mul a b = a * b

-- >>> prog2
-- 90
prog2 :: Integer
prog2 = mul (lit 6) (add (lit 5) (add (lit 4) (lit 6)))

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
