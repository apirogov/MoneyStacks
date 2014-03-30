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

import System.Exit
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import System.FilePath.Posix (isValid)

import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (showGregorian,toGregorian,fromGregorian)
import Data.Maybe (isNothing, fromJust, mapMaybe)
import Data.List (sort,intersperse)
import Control.Monad (when, unless,zipWithM_)

-- |Entry point into the moneystacks command line tool.
-- Verifies that a valid configuration file is passed, parses it,
-- checks the passed action and delegates to 'evaluate'
main :: IO ()
main = do
  args <- getArgs

  when (length args < 2)
    (putStrLn help >> exitSuccess)

  let (filename:command:rest) = args
  let valid = isValid filename
  exist <- doesFileExist filename

  unless (valid && exist) $
    error $ errInvalidFile filename
  when (command `notElem` ["stacks","show","add"]) $
    error $ errInvalidCommand command

  filestr <- readFile filename
  case parseMoneyConf filename filestr of
    Left e     -> error $ show e
    Right conf -> evaluate conf command $ if command/="add" then rest else (filename:rest)

-- |Gets the parsed config, a keyword with the action and the rest of the arguments.
-- Executes the action or reports an error.
evaluate :: MoneyConf -- ^ Already parsed configuration file
         -> String    -- ^ The action to be executed (@stacks@, @show@ or @add@)
         -> [String]  -- ^ Rest of the arguments (everything coming after the action)
         -> IO ()

-- No date passed -> use today's date
evaluate conf "stacks" [] = do
  today <- getCurrentDay
  evaluate conf "stacks" [showGregorian today]

evaluate conf "stacks" dates = do
  today <- getCurrentDay
  let dates' = sort $ mapMaybe (parseArgDate today) dates
      stacks = map (calcStacksUntil conf) dates'
  when (null dates')
    (error "No valid dates given!")
  zipWithM_ (\d s -> do
    putStr $ showGregorian d++": "
    putStr $ show s
    putStr " Total: "
    print $ sum $ map snd s
    ) dates' stacks

--no dates given -> show from beginning of month until now
evaluate conf "show" [] = do
  today <- getCurrentDay
  let (y,m,_) = toGregorian today
  evaluate conf "show" [ showGregorian $ fromGregorian y m 1
                       , showGregorian today]

-- one date assumed as start date, now as end date
evaluate conf "show" [start] = do
  today <- getCurrentDay
  evaluate conf "show" [start, showGregorian today]

evaluate conf "show" [start,end] = do
  today <- getCurrentDay
  let from = parseArgDate today start
      to   = parseArgDate today end
  when (any isNothing [from,to]) $
    error errInvalidDates
  mapM_ putStrLn $ showTransfersFromTo conf from (fromJust to)

evaluate _ "show" args = error $ errInvalidCommand $ concat $ intersperse " " ("show":args)

-- add a transfer line with current date auto-set to the specified config file
evaluate _ "add" (filename:rest) = do
  today <- getCurrentDay
  case parseArgTransfer today $ (concat.intersperse " ") rest of
    Left e  -> error $ show e
    Right t -> appendFile filename $ show t ++ "\n"

evaluate _ _ _ = error errUnknown

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

-- Error messages
errUnknown = "Unknown error. This should not have happened! If you can reproduce it, please file a bug report!"
errInvalidDates = "Invalid date(s) given!"
errInvalidFile f = "Invalid filename or file does not exist: "++f
errInvalidCommand c =  "Invalid command" ++ if not $ null c then ": "++c else ""
