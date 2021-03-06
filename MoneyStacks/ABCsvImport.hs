{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MoneyStacks.ABCsvImport
Description : Sync transactions with a CSV file generated by AqBanking-CLI
Copyright   : (c) Anton Pirogov, 2014
License     : MIT
Stability   : experimental
Portability : POSIX
-}
module MoneyStacks.ABCsvImport
where

import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Char (ord, isDigit, toUpper)
import Data.Function (on)
import Data.Csv

import MoneyStacks.Core
import MoneyStacks.Parser (parseArgDate)
import Data.Time.Calendar (fromGregorian)
import Data.Maybe (fromJust, fromMaybe)

readCSVFile = B.readFile

-- | Write imported transfers to the file specified in the config
writeImportFile :: MoneyConf -> IO ()
writeImportFile conf = case cImportFile conf of
  Nothing -> error "No Import statement specified in configuration!"
  Just filename -> writeFile filename $ unlines $ map show $ cImported conf


-- | Take ByteString with CSV data from AqBanking-CLI CSV output, return error or parsed transfers
parseCsv :: B.ByteString -> Either String [Transfer]
parseCsv str = case parsed of
  Left err  -> Left err
  Right dat -> Right $ map (toTransfer . M.fromList) $ filterCsv $ V.toList dat
  where parsed = decodeWith defaultDecodeOptions{decDelimiter=fromIntegral (ord ';')}
                            NoHeader str :: Either String (V.Vector [String])
        -- Zip header of column with each cell, fold each row to one record map
        filterCsv (header:recs) = map M.toList $ map (foldl mappify $ M.fromList []) $ map (zip header) recs
        filterCsv [] = []

        mappify rec (key,val) =  if key' `elem` ignoredKeys then rec else M.insertWith' mergeKeys key' val rec
          where key' = filter (not.isDigit) key -- multiple fields like purpose purpose1 purpose2 ... -> purpose
                mergeKeys new old = if null new then old else old ++ " " ++ new
                ignoredKeys = ["category", "transactionId", "localBankCode", "value_currency", "valutadate"]

        toTransfer m = nullTransfer {tSrc="", tDst="main", tVal=Just val, tDate=date, tText=text}
          where val  = round (read vstr :: Float)
                vstr = fromMaybe "0" $ M.lookup "value_value" m
                date = fromJust $ parseArgDate (fromGregorian 0 0 0) datestr
                datestr = map ('/' `to` '-') $ fromMaybe "0-1-1" $ M.lookup "date" m
                text = (fromMaybe "" $ M.lookup "remoteName" m) -- ++ " " ++ (fromMaybe "" $ M.lookup "purpose" m)
                a `to` b = \x -> if x==a then b else x

-- | Take set of rules and a transfer and apply the first rule matching
applyRules :: [ImportRule] -> Transfer -> Transfer
applyRules rs t = fst $ foldl applyRule (t, False) rs
  where applyRule (tr, True) _  = (tr, True) -- try to apply rule if none applied yet
        applyRule (tr, False) r = if hasMatch (rWords r) (words $ tText tr)
                                  then (tr', True)
                                  else (tr, False) -- rule does not match
          where hasMatch ws ts = True `elem` map (isMatch ws) ts -- one of the words in ts in wordlist ws?
                isMatch ws t = EQ `elem` map ((compare `on` (map toUpper)) t) ws -- word t in wordlist ws?
                tr' = tr{ tSrc = if null $ tSrc tr then "" else rStack r -- modify stackname, add desc
                        , tDst = if null $ tDst tr then "" else rStack r
                        , tText = tText tr ++ " " ++ rAddTxt r }

-- | Take an existing config and add imported transfers which are new (after last old),
-- applying modifications as stated by matching rules.
-- Assuming that there are no two transfers with the same value on the same day.
-- We can not inspect the description as it might have been modified by rules or manually.
importTransfers :: MoneyConf -> [Transfer] -> MoneyConf
importTransfers conf new = conf{cImported = merge old new''}
  where old   = cImported conf
        olast = if null old then nullTransfer else last old
        new'  = filter (\t -> t > olast || t==olast && tVal t/=tVal olast) new
        new'' = map (applyRules (cImportRules conf)) new'

