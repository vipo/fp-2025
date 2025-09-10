module Lib1
    ( examples, Command(..)
    ) where

data Dumpable = Examples
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed
data Category = SimpleCategory String 
              | NestedCategory String Category
  deriving Show

-- Root ADT representing your BNF grammar
data Command = Dump Dumpable 
             | AddBook String String Category
             | RemoveBook String
             | ListBooks
             | CheckoutBook String String
             | ReturnBook String
  deriving Show

examples :: [Command]
examples = [
    AddBook "Algorithms" "Sedgewick" (NestedCategory "Technical" (NestedCategory "Programming" (SimpleCategory "DataStructures"))),
    RemoveBook "Hobbit",
    CheckoutBook "Dune" "Alice",
    ReturnBook "Dune"
    ]
