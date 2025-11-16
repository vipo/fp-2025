-- | Notes taken by Emilija Rimšelytė
module Lessons.Lesson06 where

import Control.Concurrent (newChan, threadDelay, forkIO,readChan, writeChan, Chan)
import System.Random
import Control.Concurrent.STM
    ( TVar, writeTVar, newTVarIO, modifyTVar )
import Control.Monad.STM
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Control.Concurrent.Async
import Control.Monad (when)

-- | Entry point of a Haskell program.
-- 'putStrLn' performs an effect: it prints to the terminal.
-- In Haskell, effects live in IO — they’re not just values, they’re actions.
main :: IO ()
main = putStrLn "Hello, Haskell!"

-- | Reads a line from the terminal.
-- This is an IO action — it doesn’t give you a String directly,
-- it gives you a computation that will produce a String when run.
main' :: IO String
main' = getLine

-- | Reads and prints a line.
-- Demonstrates how to extract a value from IO using '<-' inside a 'do' block.
-- You can't escape IO in pure code — but inside 'do', you can unwrap and use.
m :: IO ()
m = do
    line <- getLine
    putStrLn line

-- | Asks the user for their name.
-- This is interactive, so it lives in IO.
-- Side effects like printing and reading are sequenced here.
queryName :: IO String
queryName = do
    putStrLn "What is your name?"
    getLine

-- | Combines IO (input/output) with pure logic.
-- The greeting is pure, but getting the name is not.
aGame :: IO ()
aGame = do
    name <- queryName
    let salute = "Hello, " ++ name ++ "!"
    putStrLn salute


-- | This won’t compile, as you can see,  
-- IO and String can’t mix so free.
-- pureF :: String -> String
-- pureF a = queryName ++ a

-- | Mixes IO with pure string manipulation.
-- You extract the name, then glue it tight,
-- But wrap it in IO to make it right.
pureF :: String -> IO String
pureF a = do
    n <- queryName
    return $ n ++ a

-- | Returns a constant string wrapped in IO.
pureF' :: IO String
pureF' = return "labas"

-- | Waits 10 seconds, then prints "Hi".
-- Demonstrates time delay — an effect, so it’s in IO.
action :: IO ()
action = do
    threadDelay 10000000
    putStrLn "Hi"

-- | Starts two concurrent threads using 'forkIO'.
-- Each runs 'action' independently.
-- Threads in Haskell are lightweight — managed by the runtime, not the OS.
threading :: IO ()
threading = do
    forkIO action
    forkIO action
    putStrLn "!"

-- | Channels let threads communicate safely.
-- Writes to the channel, after a pause,
-- Communication between threads — that’s the cause.
actionC :: Chan String -> IO ()
actionC ch = do
    threadDelay 10000000
    writeChan ch "Hi"

-- | Demonstrates inter-thread communication.
-- Two threads write to the same channel.
-- Main thread reads both messages and prints them.
threading' :: IO ()
threading' = do
    ch <- newChan
    forkIO $ actionC ch
    forkIO $ actionC ch
    v1 <- readChan ch
    v2 <- readChan ch
    putStrLn $ v1 ++ ", " ++ v2

-- | Delayed computation that returns a string.
-- Used with 'async' to run in parallel.
actionA :: String -> IO String
actionA v = do
    threadDelay 5000000
    return v

-- | Runs two async actions concurrently.
-- Waits for both to finish, then combines results.
threading'' :: IO String
threading'' = do
    a1 <- async $ actionA "First"
    a2 <- async $ actionA "Second"
    v1 <- wait a1
    v2 <- wait a2
    return $ v1 ++ " " ++ v2

-- | Transfers money between two accounts using STM.
-- STM = Software Transactional Memory.
-- It’s atomic, composable, and avoids locks.

-- If balance is low, retries the flow,
-- Wait until there's cash to go.
transfer :: TVar Integer -> TVar Integer -> Integer -> STM ()
transfer accA accB amount = do
    a <- readTVar accA
    b <- readTVar accB
    let newA = a - amount
    when (newA < 0) retry
    writeTVar accA newA
    writeTVar accB (b + amount)

-- | Demonstrates STM in action.
-- One thread tries to transfer too much,
-- Another adds funds — STM retries until it succeeds.

-- STM waits, but never breaks,
-- One thread gives, the other takes.
runTx :: IO ()
runTx = do
    a <- newTVarIO 50
    b <- newTVarIO 100
    z <- async $ atomically $ transfer a b 51
    atomically $ modifyTVar a (+10)
    _ <- wait z
    ar <- readTVarIO a
    br <- readTVarIO b
    putStrLn $ "a=" ++ show ar ++ ", b=" ++  show br
