{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib2 qualified
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )

type Repl a = HaskelineT IO a

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome! Press [TAB] for auto completion."

completer :: (Monad m) => WordCompleter m
completer _ = return []

cmd :: String -> Repl ()
cmd str = do
  case Lib2.parseCommand str of
    Left e -> liftIO $ putStrLn $ "PARSE ERROR: " ++ e
    Right (c, "") -> liftIO $ mapM_ putStrLn (Lib2.process c)
    Right (c, r) -> liftIO $ putStrLn $ "PARSED: " ++ show c ++
        ", but remaining input IS NOT fully consumed: " ++ r

main :: IO ()
main =
  evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final