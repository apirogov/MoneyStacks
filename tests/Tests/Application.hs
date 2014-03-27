module Tests.Application where
import Test.Tasty

import MoneyStacks.Application as App

import GHC.IO.Handle
import System.IO
import System.Directory
import System.Environment (withArgs)

-- |return as String what is printed to StdOut
catchOutput :: IO () -> IO String
catchOutput f = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  f
  hDuplicateTo stdout_dup stdout
  str <- readFile tmpf
  removeFile tmpf
  return str

-- |Run an application with given argument string and return the stdout output as string
runWithArgs :: String -> IO () -> IO String
runWithArgs argstr f = catchOutput $ withArgs (words argstr) f

applicationTests = testGroup "MoneyStacks.Application" [appTests]

-- TODO
appTests = testGroup "Application tests (checking CLI output)" []

