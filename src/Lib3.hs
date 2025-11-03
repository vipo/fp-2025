{-# OPTIONS_GHC -Wno-orphans #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1

import Control.Concurrent.STM.TVar(TVar)
import Control.Concurrent (Chan, readChan)

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

-- | Parses user's input.
-- Yes, this is pretty much the same parser as in Lib2
-- but with a bit different requirements:
-- 1) It must implement Functor, Applicative and Alternative
-- 2) It must NOT implement Monad, no do-notations
-- 3) pmap with andN become <$> <*>
-- 4) orElse becomes <|>
-- 5) many and many1 become many and some
-- Yes, it will be mostly a copy-paste but an easy one
-- if Lib2 was implemented correctly.
parseCommand :: Parser Lib1.Command
parseCommand = Parser $ \_ -> Left "Implement me 0"

-- | You can change the type to whatever needed. If your domain
-- does not have any state you have to make it up.
newtype State = State ()

-- Fix this accordingly
emptyState :: State
emptyState = State()

-- | Business/domain logic happens here.
-- This function makes your program actually usefull.
-- You may print if you want to print, you
-- may mutate state if needed but there must be
-- SINGLE atomically call in the function
-- You do not want to write/read files here.
execute :: TVar State -> Lib1.Command -> IO ()
execute _ _ = error "Implement me 1"

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request. It must run forever.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop c = do
  _ <- readChan c
  return $ error "Implement me 2"

-- | This function will be called periodically
-- and on programs' exit. File writes must be performed
-- through `Chan StorageOp`.
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save _ _ = return $ Left "Implement me 3"

-- | This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load _ _ = return $ Left "Implement me 4"
