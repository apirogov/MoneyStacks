{-# LANGUAGE DoAndIfThenElse #-}
{-|
Module      : MoneyStacks.Application
Description : Command line application interface for MoneyStacks
Copyright   : (c) Anton Pirogov, 2014
License     : MIT
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module MoneyStacks.Application where
import MoneyStacks.Core
import MoneyStacks.Parser

import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath.Posix (isValid)

import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (showGregorian,toGregorian,fromGregorian)
import Data.Maybe (isNothing, fromJust, mapMaybe)
import Data.List (sort)

-- TODO: Add QuickCheck/HUnit tests. Use tasty?

-- |Entry point into the moneystacks command line tool.
-- Verifies that a valid configuration file is passed, parses it,
-- checks the passed action and delegates to 'evaluate'
main :: IO ()
main = do
  args <- getArgs
  if (length args) < 2 then putStrLn help
  else do
       let (filename:command:rest) = args
       let valid = isValid filename
       exist <- doesFileExist filename
       if not valid || not exist then putStrLn $ filename++": Invalid filename or file does not exist!"
       else if command `notElem` ["stacks","show","add"] then putStrLn $ command++": Invalid command!"
       else do filestr <- readFile filename
               case parseMoneyConf filename filestr of
                    Left e -> putStrLn $ prettyParseError e
                    Right conf -> evaluate conf command $ if command/="add" then rest else (filename:rest)

-- |Gets the parsed config, a keyword with the action and the rest of the arguments.
-- Executes the action or reports an error.
evaluate :: MoneyConf -- ^ Already parsed configuration file
         -> String    -- ^ The action to be executed (@stacks@, @show@ or @add@)
         -> [String]  -- ^ Rest of the arguments (everything coming after the action)
         -> IO ()
evaluate conf "stacks" [] = do
  today <- getCurrentDay -- No date passed -> use today's date
  evaluate conf "stacks" [showGregorian today]

evaluate conf "stacks" dates = do
  today <- getCurrentDay
  let dat = sort $ mapMaybe (parseDate today) dates
  if null dat
  then putStrLn "No valid dates given!"
  else mapM_ putStrLn $ zipWith (++)
                                (map (\x -> showGregorian x++": ") dat)
                                (map (\s -> show s ++ " Total: " ++ (show $ foldl (\acc (_,x)->acc+x) 0 s))
                                                                   (map (calcStacksUntil conf) dat) )

evaluate conf "show" [] = do
  today <- getCurrentDay --no dates given -> show from beginning of month until now
  let (y,m,_) = toGregorian today
  evaluate conf "show" [showGregorian$fromGregorian y m 1,showGregorian today]

evaluate conf "show" [start] = do
  today <- getCurrentDay -- one date assumed as start date, end = now
  evaluate conf "show" [start, showGregorian today]

evaluate conf "show" [start,end] = do
  today <- getCurrentDay
  let from = parseDate today start
      to   = parseDate today end
  if any isNothing [from,to]
  then putStrLn "Invalid date(s) given!"
  else mapM_ putStrLn $ showTransfersFromTo conf from (fromJust to)
evaluate _ "show" _ = putStrLn "Invalid command format!"

evaluate _ "add" (filename:rest) = do -- add a transfer line with current date auto-set to the specified config file
  today <- getCurrentDay
  case argTransfer today $ foldl (\x y->x++" "++y) "" rest of
    Left e -> putStrLn $ prettyParseError e
    Right t -> appendFile filename $ show t ++ "\n"

evaluate _ _ _ = putStrLn "Some strange error. This should not have happened!"

-- |Usage help text
help = unlines [
        "Usage:"
      , "moneystacks FILE ACTION [OPTIONS]", "", "FILE: obligatory valid MoneyStacks configuration file", ""
      , "ACTIONs:", ""
      , "stacks [DATE*]:","\tShow stack distribution on given DATEs",""
      , "show [START_DATE [END_DATE]]:","\tShow transfers. default: beginning of month until now",""
      , "add VALUE [from SOURCE_STACK to DEST_STACK : DESCRIPTION_TEXT]:"
      , "\tAdd a new transfer line to FILE, unset options will be set to default" ]

-- |returns current Day
getCurrentDay = getCurrentTime >>= \t -> return $ utctDay t

