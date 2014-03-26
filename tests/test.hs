module Main where
import Test.Tasty

import Tests.Core
import Tests.Parser
import Tests.Application

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [coreTests, parserTests, applicationTests]
