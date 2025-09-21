{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- | Parses user's input.
-- The function must be implemented and must have tests.
parseCommand :: Parser Lib1.Command
parseCommand _ = Left "Not implemented"

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

-- | You have to make your Command an instance of ToCliCommand class.
-- Please remove all custom Show instances of Command ADT and
-- use "deriving Show" only.
instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand _ = "Not implemented"

-- | You have to make your Command an instance of Eq class.
-- Usage of "deriving Eq" is forbidden.
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  _ == _ = False
