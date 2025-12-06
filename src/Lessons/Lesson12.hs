-- | Notes taken by Deimantė Davidavičiūtė
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
module Lessons.Lesson12 (MyDomain, MyDomainAlgebra(..), calculte, store, restore) where

-- | Free monad over a small domain-specific algebra.
-- 'Free f a' lets us build programs where effects are described
-- by the functor 'f' and interpreted later.
import Control.Monad.Free (Free (..))
import Control.Monad.Trans.State.Strict (State, get, put, runState)

import Lessons.Lesson11 (Expr(..), eval)

-- | The algebra of our domain:
-- - 'Calculate Expr (Integer -> next)': evaluate an arithmetic expression,
--    then pass the resulting Integer to the continuation.
-- - 'Store Integer (() -> next)': store a number and continue.
-- - 'Restore (Integer -> next)': fetch the last stored number and continue.
data MyDomainAlgebra next = Calculate Expr (Integer -> next)
                          | Store Integer (() -> next)
                          | Restore (Integer -> next)

-- | Our algebra must be a Functor to build a Free monad.
-- Demonstration of functorial mapping (not tied to our type):
--
-- >>> fmap (+5) (Just 5)
-- Just 10

instance Functor MyDomainAlgebra where
  fmap :: (a -> b) -> MyDomainAlgebra a -> MyDomainAlgebra b
  fmap f (Calculate e next) = Calculate e (\a -> f (next a))
  fmap f (Store i next) = Store i (\a -> f (next a))
  fmap f (Restore next) = Restore (\a -> f (next a))

-- | The Free monad over 'MyDomainAlgebra'.
type MyDomain a = Free MyDomainAlgebra a

-- | Lift a 'Calculate' instruction into the Free program.
calculte :: Expr -> MyDomain Integer
calculte e = Free (Calculate e Pure)

-- | Lift a 'Store' instruction.
store :: Integer -> MyDomain ()
store i = Free (Store i Pure)

-- | Lift a 'Restore' instruction.
restore :: MyDomain Integer
restore = Free (Restore Pure)

-- | A sample program composed of our domain actions.
-- It:
-- 1) Calculates an expression, yielding 'r'.
-- 2) Restores a value 'v0'.
-- 3) Stores 'r' and then stores 42.
-- 4) Restores 'v1'.
-- 5) Returns the sum.
myProgram :: MyDomain Integer
myProgram = do
  r <- calculte $ Neg (Add (Lit 5) (Add (Lit 4) (Lit 6)))
  v0 <- restore
  store r
  store 42
  v1 <- restore
  return $ r + v0 + v1

-- | An interpreter that runs the program in IO.
-- Effects are implemented as printing/prompts and pure evaluation.
runInIO :: MyDomain a -> IO a
runInIO (Pure v) = return v
runInIO (Free step) = do
  next <- runStep step
  runInIO next
  where
    runStep :: MyDomainAlgebra a -> IO a
    runStep (Calculate exp next) = return $ next (eval exp)
    runStep (Store i next) = do
      putStrLn $ "Please enter this number when asked: " ++ show i
      return $ next ()
    runStep (Restore next) = do
      putStrLn "Please enter the last number"
      s <- getLine
      return $ next (read s)

-- | Interpreter that runs the program in pure 'State Integer'.
-- The state holds the latest stored number.
--
-- >>> runState (runInState myProgram) 0
-- (27,42)
runInState :: MyDomain a -> State Integer a
runInState (Pure v) = return v
runInState (Free step) = do
  next <- runStep step
  runInState next
  where
    runStep :: MyDomainAlgebra a -> State Integer a
    runStep (Calculate exp next) = return $ next (eval exp)
    runStep (Store i next) = put i >> return (next ())
    runStep (Restore next) = get >>= (return . next)


-- | Compose two programs applicatively, sharing the same interpreter/state.
--
-- >>> runState (runInState myTwoPrograms) 0
-- ((27,69),42)
myTwoPrograms :: MyDomain (Integer, Integer)
myTwoPrograms = (,) <$> myProgram <*> myProgram
