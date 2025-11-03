-- import Control.Concurrent (Chan)
-- import Data.String (IsString)
-- import Lessons.Lesson03 (a1)
-- import GHC.Conc
-- import GHC.Arr (accumArray)
-- import GHC.TopHandler (runIOFastExit)
-- main :: IO ()
-- main = putStrLn "Hello, Haskell!"

-- --all bussiness logic in functional core in trhe functions and all io in imperative shell
-- m :: IO ()
-- m = do
--   line <- getLine
--   putStrLn line
-- -- if we want to extract value from IO we need to use <- inside do block which means FIND
-- -- and then it compiles
-- -- putStrLn returns ...
-- -- getLine is IO notation ans line is a string

-- aGame :: IO
-- aGame = do
--     putStrLn "what is your name" --imperative shell
--     name <- getLine
--     let salute = "Hello, " ++ name ++ "!" --functional core
--     putStrLn salute

-- queryName :: IO String
-- queryName = do
--     putStrLn "What is your name?"
--     getLine

-- pureF :: String -> String
-- pureF a = queryName ++ a -- does not compile, we need tooo

-- pureF' :: String -> IO String --we change return type to IO String
-- pureF' a = 
--     do --we add do
--         n <- queryName --we extract value from IO String
--        -- n ++ a -- now it (does not?) compiles
--        -- because we extracted n as a strig and a is a string
--         return $ n ++ a -- we need to use return to wrap it back to IO String
--         --expected io string but got a string


--         ---DRAMATISK PAUZE---
-- pureF'' :: IO String
-- pureF'' a = return "labas" -- we can use return to wrap pure values into IO context

-- --returns io unit if it has nothing meaningful to return

-- action :: IO ()
-- action = do
--     threadDelay 10000000
--     putStrLn "10 seconds passed"

-- --green threads
-- --lighweight
-- --runtime controls it
-- --can have thousands of them
-- --can be used for concurrency
-- --done within a single process


-- threading :: IO ()
-- threading = do
--     forkIO action --forkIO starts a new thread
--     forkIO action --we need sth to run the thread on
--     putStrLn

-- --they compete for time on a single core
-- --but we can use channel to communicate between threads

-- actionC :: Chan String -> IO ()
-- actionC = do
--     threadDalay 1000000
--     writeChan chan "10 seconds passed"

-- threadingC :: IO ()
-- threadingC = do
--     chan <- newChan --extract ch from IO context
--     forkIO $ actionC chan
--     forkIO $ actionC chan
--     v1 <- readChan chan --returns IO String so we can extract it with <-
--     v2 <- readChan chan
--     putStrLn $ v1 ++ ", " ++ v2

-- --its ugly its not good at all

-- actionA :: String -> IO String
-- actionA = do
--     threadDelay 1000000
--     return "HI"
    
-- threadingA :: IO String
-- threadingA = do
--     a1 <- async $ actionA "First" --async starts a new thread and returns an Async handle44
--     --its async strings so need to wait
--     v1 <- wait a1
--     v2 <- wait a2
--     --they all run at the same time 
--     a2 <- async $ actionA "Second"
--     return $ a1 ++ ", " ++ a2

-- --we have 20 of time!!!!!!!
-- --fingie
-- --thingie
-- ---dingie
-- --bongie
-- --zongie

-- --sing
-- --king
-- --ring
-- --ping
-- --wing
-- --ming
-- --string
-- --bring
-- --fling
-- --cling
-- --sling
-- --spring
-- --swing
-- --twing
-- --zwing
-- --bling
-- --gring
-- --fring
-- --dring
-- --cring
-- --prong
-- --strong
-- --wrong
-- --throng
-- --wring
-- --syring
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing
-- --stringing


-- --STM - software transactional memory
-- --like database transactions
-- --all operations inside transaction are atomic
-- --if one fails all fail
-- --no locks no deadlocks
-- --composable
-- --easy to reason about
-- --slower than locks
-- --not suitable for all problems

-- transfer :: TVar Integer -> TVar Integer -> Int -> STM ()
-- --diva
-- transfer accA accB amount = do
--     a <- readTVar accuA 
--     b <- readTVar accB
--     writeTVar accA (a - amount)
--     writeTVar accB (b + amount)
--     return ()

-- runTx:: IO ()
-- runTx = do
--     a <- newTVarIO 50
--     b <- newTVarIO 100
--     atomically $ transfer a b 5
--     ar <- readTVarIO a
--     br <- readTVarIO b
--     putStrLn $ show ar ++ ", " ++ show br
