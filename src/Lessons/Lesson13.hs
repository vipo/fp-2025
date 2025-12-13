-- | Notes taken by Jonas Grybė
--
-- Lambda Calculus Foundations:
-- People wanted to formalize computations - what are computations, how do they work?
-- Lambda calculus emerged as a very primitive language that still allows calculation.
-- It's amazing because it detaches computations from physical things.

-- This lesson shows how monadic do-notation gets mechanically transformed
-- into continuation-passing style through 14 refactoring steps.

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
module Lessons.Lesson13 () where

import Control.Monad.Free (Free (..))
import Control.Monad.Trans.State.Strict (State, get, put, runState)

import Lessons.Lesson11 (Expr(..), eval)

import Lessons.Lesson12 (MyDomain, MyDomainAlgebra(..), calculte, restore, store)

e :: Expr
e = Neg (Add (Lit 5) (Add (Lit 4) (Lit 6)))

-- | Original program in do-notation.
-- Calls restore, bind, bind with argument v, then store, bind (ignoring result), then return.
myProgram :: MyDomain Integer
myProgram = do
  r <- calculte e
  v <- restore
  store r
  return  v

-- | Step 0: Desugar do-notation into explicit bind (>>=) operators.
-- Shows the structure hidden by do-notation syntax.
myProgram0 :: MyDomain Integer
myProgram0 = calculte e >>= (\r ->
  restore >>= (\v ->
    store r >>= (\_ ->
      return v
      ))
  )

-- | Step 1: Replace method names (calculte, restore, store) with their actual implementations.
-- Now we see the Free constructors (Calculate, Restore, Store) explicitly.
myProgram1 :: MyDomain Integer
myProgram1 = Free (Calculate e Pure) >>= (\r ->
  Free (Restore Pure) >>= (\v ->
    Free (Store r Pure) >>= (\_ ->
      return v
      ))
  )

-- | Step 2: Apply the Monad instance rule: Free m >>= f = Free (fmap (>>= f) m)
-- The first bind gets transformed, revealing the fmap.
myProgram2 :: MyDomain Integer
myProgram2 = Free (
  fmap
    (>>= (\r ->
      Free (Restore Pure) >>= (\v ->
        Free (Store r Pure) >>= (\_ ->
          return v
          )
        )
      ))
    (Calculate e Pure)
  )

-- | Step 3: Replace fmap with its realization from the Functor instance.
-- fmap f (Calculate e next) = Calculate e (\a -> f (next a))
myProgram3 :: MyDomain Integer
myProgram3 = Free (
  Calculate e (\a ->
    (>>= (\r ->
      Free (Restore Pure) >>= (\v ->
        Free (Store r Pure) >>= (\_ ->
          return v
          )
        )
      )) (Pure a)
    )
  )

-- | Step 4: Move Pure a from the end to the front as the first argument of bind.
-- Prepares for applying the Pure >>= f = f a rule.
myProgram4 :: MyDomain Integer
myProgram4 = Free (
  Calculate e (\a ->
    (Pure a) >>= (\r ->
      Free (Restore Pure) >>= (\v ->
        Free (Store r Pure) >>= (\_ ->
          return v
          )
        )
      )
    )
  )

-- | Step 5: Apply Pure >>= f = f a rule from Monad instance.
-- The 'a' from Calculate goes to Pure, becomes 'r', so we remove Pure and just use 'r'.
myProgram5 :: MyDomain Integer
myProgram5 = Free (
  Calculate e (\r ->
    Free (Restore Pure) >>= (\v ->
      Free (Store r Pure) >>= (\_ ->
        return v
        )
      )
    )
  )

-- | Step 6: Deal with the second bind. Apply Free m >>= f = Free (fmap (>>= f) m).
-- We have Free (Restore Pure) and a function on the right.
myProgram6 :: MyDomain Integer
myProgram6 = Free (
  Calculate e (\r ->
    Free (
      Restore (\a -> (>>= (\v ->
        Free (Store r Pure) >>= (\_ ->
          return v
        ))
      ) (Pure a))
    )
  ))


-- | Step 7: Replace fmap with the Restore realization.
-- fmap f (Restore next) = Restore (\a -> f (next a))
myProgram7 :: MyDomain Integer
myProgram7 = Free (
  Calculate e (\r ->
    Free (
      Restore (\a -> (Pure a >>= (\v ->
        Free (Store r Pure) >>= (\_ ->
          return v
        ))
      ))
    )
  ))

-- | Step 8: Apply Pure a >>= f = f a again.
-- The function already had name 'v', so we simplify by removing brackets.
myProgram8 :: MyDomain Integer
myProgram8 = Free (
  Calculate e (\r ->
    Free (
      Restore (\v ->
        Free (Store r Pure) >>= (\_ ->
          return v
        )
      )
    )
  ))

-- | Step 9: We have Free and bind again, apply the transformation rule.
-- Free m >>= f = Free (fmap (>>= f) m)
myProgram9 :: MyDomain Integer
myProgram9 = Free (
  Calculate e (\r ->
    Free (
      Restore (\v ->
        Free (fmap (>>= (\_ ->
          return v
        )) (Store r Pure)) 
      )
    )
  ))

-- | Step 10: Replace fmap with Store realization.
-- fmap f (Store i next) = Store i (\a -> f (next a))
myProgram10 :: MyDomain Integer
myProgram10 = Free (
  Calculate e (\r ->
    Free (
      Restore (\v ->
        Free (
          Store r (\a -> (>>= (\_ ->
            return v
          )) (Pure a))
      )
    )
  )))

-- | Step 11: Move Pure a to the front of bind again.
-- Take the argument from the end and put it in front.
myProgram11 :: MyDomain Integer
myProgram11 = Free (
  Calculate e (\r ->
    Free (
      Restore (\v ->
        Free (
          Store r (\a -> (Pure a >>= (\_ ->
            return v
          )))
      )
    )
  ))
  )

-- | Step 12: Apply Pure a >>= f = f a using the Monad rule.
-- Simplifies the bind with Pure.
myProgram12 :: MyDomain Integer
myProgram12 = Free (
  Calculate e (\r ->
    Free (
      Restore (\v ->
        Free (
          Store r (\a -> ((\_ ->
            return v
          ) a ))
      )
    )
  ))
  )

-- | Step 13: The 'a' comes as an argument, but we ignore it (use _) and return v.
-- Simplify to a function with no 'a'.
myProgram13 :: MyDomain Integer
myProgram13 = Free (
  Calculate e (\r ->
    Free (
      Restore (\v ->
        Free (
          Store r (\_ ->
            return v
          )
      )
    )
  ))
  )

-- | Step 14: Replace 'return' with 'Pure' (return = pure = Pure in our Monad).
-- Final form in continuation-passing style!
-- We calculate e, get result r, call Free with Restore, which gives us v,
-- pass to Store, ignore result, and end with Pure v signaling completion.
myProgram14 :: MyDomain Integer
myProgram14 = Free (
  Calculate e (\r ->
    Free (
      Restore (\v ->
        Free (
          Store r (\_ ->
            Pure v
          )
      )
    )
  ))
  )

-- | The Free monad structure and its instances (for reference):
-- data Free f a = Pure a | Free (f (Free f a))
--
-- Pure signals there's nowhere to go further.
-- Free says "here we go next" - contains the next step.
--
-- The result is nested as needed, passing arguments as far as needed.
-- When we run the interpreter: if it's Free, it contains the next step;
-- if it's Pure, it's the final result.
--
-- Performance note: This transformation reveals why Free monad programs aren't very fast.
-- We had only 3 expressions but performed 14 steps - the complexity is roughly quadratic
-- (n expressions → n² steps). Control.Monad.Free has various interpreters to solve
-- the speed issue. Sometimes computation isn't quadratic if the tree only needs to be
-- constructed once, making it more linear.
-- The rollout of this procedure is a huge and known problem that is actively being solved.


-- | Functor and Monad instance rules used throughout the transformations above.
-- fmap f (Calculate e next) = Calculate e (\a -> f (next a))
-- fmap f (Store i next) = Store i (\a -> f (next a))
-- fmap f (Restore next) = Restore (\a -> f (next a))

-- instance Functor f => Monad (Free f) where
--   Pure a >>= f = f a
--   Free m >>= f = Free (fmap (>>= f) m)