{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib4 where

import qualified Lib1
import Test.QuickCheck (Arbitrary, Gen, arbitrary)

import Control.Monad.Trans.State.Strict (State)
import Control.Monad.Trans.Except (ExceptT)

type ErrorMsg = String
type Input = String
type Parser = ExceptT ErrorMsg (State Input)

-- | Parses user's input.
-- Yes, yes, yes. This is pretty much the same parser as in Lib3
-- It will be mostly a copy-paste because all <|>, <$>, <*> work
-- out of the box and only terminal (leaves) parsers will be changed.
parseCommand :: Parser Lib1.Command
parseCommand = error "Implement me and use on server side"

-- | This generates arbitrary (a.k.a random) commands for tests.
instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary = error "Implement me"
