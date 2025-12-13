{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
module Lessons.Lesson13 () where

import Control.Monad.Free (Free (..))
import Control.Monad.Trans.State.Strict (State, get, put, runState)

import Lessons.Lesson11 (Expr(..), eval)

import Lessons.Lesson12 (MyDomain, MyDomainAlgebra(..), calculte, restore, store)

e :: Expr
e = Neg (Add (Lit 5) (Add (Lit 4) (Lit 6)))

myProgram :: MyDomain Integer
myProgram = do
  r <- calculte e
  v <- restore
  store r
  return  v

myProgram0 :: MyDomain Integer
myProgram0 = calculte e >>= (\r ->
  restore >>= (\v ->
    store r >>= (\_ ->
      return v
      ))
  )

myProgram1 :: MyDomain Integer
myProgram1 = Free (Calculate e Pure) >>= (\r ->
  Free (Restore Pure) >>= (\v ->
    Free (Store r Pure) >>= (\_ ->
      return v
      ))
  )

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

-- data Free f a = Pure a | Free (f (Free f a))

-- fmap f (Calculate e next) = Calculate e (\a -> f (next a))
-- fmap f (Store i next) = Store i (\a -> f (next a))
-- fmap f (Restore next) = Restore (\a -> f (next a))

-- instance Functor f => Monad (Free f) where
--   Pure a >>= f = f a
--   Free m >>= f = Free (fmap (>>= f) m)
