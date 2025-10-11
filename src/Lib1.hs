module Lib1
    ( examples, Command(..), Category(..), Dumpable(..)
    ) where
--import Lessons.Lesson01 (s)



-- instance Show Dumpable where
--   show Examples = "examples"

-- instance Show Command where
--   show (Dump d) = "dump " ++ show d
--   show (AddBook title author cat) = "add book " ++ show title ++ " " ++ show author ++ " " ++ show cat
--   show (RemoveBook title) = "remove book " ++ show title  
--   show ListBooks = "list books"
--   show (CheckoutBook title user) = "checkout book " ++ show title ++ " " ++ show user
--   show (ReturnBook title) = "return book " ++ show title

-- instance Show Category where
--   show (SimpleCategory s) = show s
--   show (NestedCategory s n) = show s ++ "(" ++ show n ++ ")"

data Dumpable = Examples
  deriving (Show, Eq)

data Category = SimpleCategory String 
              | NestedCategory String Category
  deriving (Show, Eq)


data Command = Dump Dumpable
             | AddBook String String Category
             | RemoveBook String
             | ListBooks
             | CheckoutBook String String
             | ReturnBook String
  deriving Show

examples :: [Command]
examples = [
    Dump Examples,
    AddBook "Algorithms" "Sedgewick" (NestedCategory "Technical" (NestedCategory "Programming" (SimpleCategory "DataStructures"))),
    AddBook "Dune" "Herbert" (SimpleCategory "Fiction"),
    RemoveBook "Hobbit",
    CheckoutBook "Dune" "Alice",
    ReturnBook "Dune"
    ]
    