module Lessons.Lesson06 where

import Control.Concurrent (newChan, threadDelay, forkIO,readChan, writeChan, Chan)
import System.Random
import Control.Concurrent.STM
    ( TVar, writeTVar, newTVarIO, modifyTVar )
import Control.Monad.STM
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Control.Concurrent.Async
import Control.Monad (when)

-- | Up until now, all functions we wrote were pure expressions. 
--   Pure functions always return the same result for the same input and have no side effects. 
--   However, in real programs, we often need to interact with the external world, such as reading input, printing to the terminal, or communicating with files and networks. 
--   This is where IO comes in — it allows us to perform side effects in a controlled way.
--   In Haskell, every function that interacts with the outside world must return an IO type.
main :: IO ()
main = putStrLn "Hello World"

-- | The function 'getLine' reads a line from the terminal. 
--   Its type is IO String, meaning it produces a String, but only when executed within the IO context.
main' :: IO String
main' = getLine

-- | All IO operations are considered “unsafe” because they depend on external factors that might fail (for example, the user may not input anything). 
--
--   Functional programming encourages us to keep the pure logic of a program
-- — the part that only performs calculations — separate from the parts that interact with the outside world, 
-- such as databases, files, user input, networks, or random number generation.
-- This outer layer that performs these side effects is often called the imperative shell, 
-- while the pure, predictable part of the code is called the functional core.
--
--   To extract values from an IO action, we use the 'do' notation.
m :: IO ()
m = do
    line <- getLine
    putStrLn line

-- | This function demonstrates user interaction. It asks the user for their name and returns it. ??????
queryName :: IO String
queryName = do
    putStrLn "What is your name"
    getLine

-- | Here we combine multiple IO actions to create a small interactive program.
--
--   This is a small program that performs an action in the real world
--   (printing to the terminal in this case), but does not return a value.
--
--   The 'do' block allows us to run several IO actions one after another in sequence.
--
--   The '<-' syntax extracts a pure value (String) from an IO action (IO String).
--
--   The 'let' keyword is used to define pure values inside a 'do' block
--   (it does not perform IO, it only binds expressions).
--
--   The function 'putStrLn' has the type: String -> IO ().
aGame :: IO ()
aGame = do
    name <- queryName
    let salute = "Hello, " ++ name
    putStrLn salute

-- | The last line of a 'do' block must be an IO action or a returned value.
--   The keyword 'return' in Haskell does not exit a function like in imperative languages.
--   Instead, it takes a pure value and wraps it inside an IO container.
pureF :: String -> IO String
pureF a = do 
    n <- queryName
    return $ n ++ a

-- | This is a simple example of using 'return' to wrap a pure value into IO.
pureF' :: IO String
pureF' = return "labas"

-- | The function 'putStrLn' produces an IO () value, not a pure value.
--   Therefore, it cannot be assigned to something of type (), because IO and pure () are not the same.
--   The code below does not compile for that reason.
--
--   p :: ()
--   p = putStrLn "labas"

-- | The unit type '()' represents a value that carries no information. 
--   It is similar to 'void' in C-like languages, but it is still a valid value. 
--   The function 'threadDelay' pauses the current thread for a given number of microseconds. 
--   One million microseconds equals one second.
action :: IO ()
action = do
    threadDelay 10000000
    putStrLn "Hi"

-- | A process is a running instance of a program that has its own memory space.
--   A thread, on the other hand, runs within a process and shares its memory with other threads.
--
--   In Haskell, we use lightweight or “green” threads that are managed by the Haskell runtime system, not by the operating system.
--   This makes them very cheap to create and allows us to run thousands of them efficiently.
--
--   The function 'forkIO' launches a new lightweight thread that executes a given IO action.
--   In this example, two threads run the 'action' function concurrently,
--   while the main thread immediately prints "!" without waiting for them to finish.
threading :: IO ()
threading = do
    forkIO action
    forkIO action
    putStrLn "!"

-- | Channels (Chan) are communication mechanisms that allow threads to send and receive messages safely.
--   In this example, a thread waits for ten seconds and then writes a message into the channel.
actionC :: Chan String -> IO ()
actionC ch = do
    threadDelay 10000000
    writeChan ch "Hi"

-- | The 'forkIO' function launches a new thread that runs an IO action.
--   The function 'newChan' creates a new channel that multiple threads can use to communicate.
--   'readChan' blocks until a value is written to the channel.
threading' :: IO ()
threading' = do
    ch <- newChan
    forkIO $ actionC ch
    forkIO $ actionC ch
    v1 <- readChan ch
    v2 <- readChan ch
    putStrLn $ v1 ++ ", " ++ v2

-- | This helper function simulates a time-consuming IO action.
actionA :: String -> IO String
actionA v = do
    threadDelay 5000000
    return v

-- | In this example, both actions run concurrently using 'async'. 
--   The 'wait' function blocks until the corresponding asynchronous task completes.
--   The results are then combined into one string.
--
-- Because both tasks started together and take the same amount of time (5 seconds),
-- both wait calls will complete almost simultaneously 
-- — the total runtime is about 5 seconds, not 10.
threading'' :: IO String
threading'' = do
    a1 <- async $ actionA "First"
    a2 <- async $ actionA "Second"
    v1 <- wait a1
    v2 <- wait a2
    return $ v1 ++ " " ++ v2

-- | STM (Software Transactional Memory) allows us to perform atomic memory operations 
--   that behave like database transactions.
--   The type 'TVar' represents a transactional variable that can be read and modified 
--   safely inside STM.
--
--   The writes made inside an STM block are staged as part of the transaction.
--   They are not visible to other threads until the transaction commits successfully.
--
--   The function 'retry' automatically blocks and restarts the transaction 
--   whenever one of the involved 'TVar's (such as 'accA' or 'accB') changes.
--
--   The 'transfer' function moves a given amount from one account to another atomically.
--   If the source account balance becomes negative, the transaction is rolled back 
--   and retried later when the data changes.
transfer :: TVar Integer -> TVar Integer -> Integer -> STM () 
transfer accA accB amount = do
    a <- readTVar accA
    b <- readTVar accB
    writeTVar accA (a - amount)
    writeTVar accB (b + amount)
    newA <- readTVar accA
    when (newA < 0) retry

-- | The function 'runTx' demonstrates how to run STM transactions from within the IO world.
--
--   The variable 'z' is an 'Async' handle — a reference to a background task running in another thread.
--   It is returned immediately when the asynchronous computation starts, 
--   not after the transaction has finished executing.
--
--   The 'atomically' function executes an STM action safely, ensuring that 
--   all operations inside it either succeed together or fail together as a unit.
--
--   In this example, the transaction inside the async thread initially calls 'retry'
--   and therefore waits until one of the involved 'TVar's is modified. 
--   The handle 'z' already exists at this point, even though the transaction itself is paused.
--
--   When the main thread modifies 'a' using 'modifyTVar', 
--   the STM runtime automatically wakes the waiting transaction, 
--   allowing it to complete successfully. 
--   Finally, 'wait z' blocks until the background task finishes, 
--   and the final account balances are printed to the console.
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