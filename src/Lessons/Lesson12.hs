{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
module Lessons.Lesson12 (MyDomain, MyDomainAlgebra(..), calculte, store, restore) where

import Control.Monad.Free (Free (..))
import Control.Monad.Trans.State.Strict (State, get, put, runState)

import Lessons.Lesson11 (Expr(..), eval)

data MyDomainAlgebra next = Calculate Expr (Integer -> next)
                          | Store Integer (() -> next)
                          | Restore (Integer -> next)

-- >>> fmap (+5) (Just 5)
-- Just 10

instance Functor MyDomainAlgebra where
  fmap :: (a -> b) -> MyDomainAlgebra a -> MyDomainAlgebra b
  fmap f (Calculate e next) = Calculate e (\a -> f (next a))
  fmap f (Store i next) = Store i (\a -> f (next a))
  fmap f (Restore next) = Restore (\a -> f (next a))

type MyDomain a = Free MyDomainAlgebra a

calculte :: Expr -> MyDomain Integer
calculte e = Free (Calculate e Pure)

store :: Integer -> MyDomain ()
store i = Free (Store i Pure)

restore :: MyDomain Integer
restore = Free (Restore Pure)

myProgram :: MyDomain Integer
myProgram = do
  r <- calculte $ Neg (Add (Lit 5) (Add (Lit 4) (Lit 6)))
  v0 <- restore
  store r
  store 42
  v1 <- restore
  return $ r + v0 + v1

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


-- >>> runState (runInState myTwoPrograms) 0
-- ((27,69),42)
myTwoPrograms :: MyDomain (Integer, Integer)
myTwoPrograms = (,) <$> myProgram <*> myProgram
