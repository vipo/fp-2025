{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Control.Monad.Trans.State.Strict (runState)
import Control.Monad.Trans.Except (runExceptT)

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Lib4 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [lib1Tests, lib2Tests, lib3Tests, lib4Tests]

lib1Tests :: TestTree
lib1Tests = testGroup "Lib1 tests"
  [ testCase "List of examples contains a few entries" $
      length Lib1.examples >= 4 @?= True
  ]

lib2Tests :: TestTree
lib2Tests = testGroup "Lib2 tests" (
  map (\e -> testCase (show e ++ " is parsed") $
    Lib2.parseCommand (Lib2.toCliCommand e) @?= Right (e, "")
  ) Lib1.examples)

lib3Tests :: TestTree
lib3Tests = testGroup "Lib3 tests" (
  map (\e -> testCase (show e ++ " is parsed") $
    Lib3.runParser Lib3.parseCommand (Lib2.toCliCommand e) @?= Right (e, "")
  ) Lib1.examples)

lib4Tests :: TestTree
lib4Tests = testGroup "Lib4 tests" [
  QC.testProperty "Command is parsed" $
    \command -> runState (runExceptT Lib4.parseCommand) (Lib2.toCliCommand command)
      == (Right command, "")
  ]
