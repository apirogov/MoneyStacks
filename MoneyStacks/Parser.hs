{-# LANGUAGE NoMonomorphismRestriction #-}
{-|
Module      : MoneyStacks.Parser
Description : Parser for MoneyStacks configuration files and some CLI arguments
Copyright   : (c) Anton Pirogov, 2014
License     : MIT
Stability   : experimental
Portability : POSIX

Also see <example_config.txt> to see how a configuration file has to look like.
-}
module MoneyStacks.Parser (
  parseMoneyConf
, parseImported
, parseArgDate
, parseArgTransfer
) where
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos (initialPos)
import Data.Time.Calendar
import Data.Maybe (isNothing, catMaybes)
import Data.List ((\\), foldl')

import MoneyStacks.Core

-- |Each line of the configuration contains an information atom
-- which is later to be merged into the MoneyConf
data ConfAtom = AtomEmpty | AtomT Transfer | AtomM [Macro] | AtomO Day
              | AtomI String  -- ^Other filename
              | AtomA String [String] String -- ^Stack, words, description
                deriving (Show,Eq)
isOrigin (AtomO _) = True
isOrigin _ = False
isImport (AtomI _) = True
isImport _ = False
isTransfer (AtomT _) = True
isTransfer _ = False

-- |A file is just the sum of its lines
file = do atoms <- sepEndBy line newline
          notFollowedBy alphaNum
          return atoms

-- |Parse a configuration line. It might be just whitespace and comment or have an expression
line = try emptyline <|> do
            s <- statement
            optional comment
            return s

-- |This accepts just a complete whitespace line with an optional comment.
emptyline = do
  try comment <|> do
              wordsep
              notFollowedBy alphaNum
              return ""
  return AtomEmpty <?> "whitespace"

-- |A statement begins with a basic (Transfer,Macro) or sugared verb (some keyword),
-- indentation and whitespace does not matter
statement = wordsep >> choice (fmap try [origin,transfer,macro
                             ,verb_init,verb_move,verb_in,verb_out
                             ,verb_regular,verb_save,verb_limit,abc_import,abc_auto])

origin = do string "Origin"
            wordsep1
            d <- date
            return $ AtomO d
            <?> "Origin"

-- |Tries to read a value (number or the word everything)
valOrAll = choice [try $ string "everything" >> return Nothing, value >>= return.Just]

transfer = do string "Transfer"
              wordsep1
              val <- valOrAll
              opts <- transferOptsWith transferMods
              let trans = applyTOpts opts nullTransfer{tVal=val}
              return $ AtomT trans
              <?> "Transfer"

-- | List of all modifiers / arguments of a transfer
transferMods = ["from","on","to",":"]

-- | Read a series of allowed modifiers and values for a transfer.
transferOptsWith keywords =
               do wordsep
                  sepEndBy (do
                    key <- choice $ fmap (try.string) keywords
                    wordsep1
                    val <- if key=="on"
                          then date >>= return.Left
                          else if key==":"
                                then many1 (noneOf "#\n\r") >>= return.Right
                                else name >>= return.Right
                    return (key, val)) wordsep1

applyTOpt :: Transfer -> (String,Either Day String) -> Transfer
applyTOpt t ("from", Right n) = t {tSrc=n}
applyTOpt t (":", Right n) = t {tText=n}
applyTOpt t ("to", Right n) = t {tDst=n}
applyTOpt t ("on", Left d) = t {tDate=d}
applyTOpt t _ = t -- everything else is unknown
-- |apply list of modifiers to a transfer (helper function for different keywords)
applyTOpts = flip $ foldl' applyTOpt

macro = do string "Macro"
           wordsep1
           val <- valOrAll
           opts <- macroOptsWith macroMods
           let m = foldl' applyMOpt nullMacro{mVal=val} opts
           return $ AtomM [m]
           <?> "Macro"

-- | Helper data type to hold a parsed macro modifier
data MOpt = MOptS String | MOptI Integer | MOptD Day
-- | List of all modifiers / arguments of a macro
macroMods = ["from","to","on","start","end","every",":"]

-- | Read a series of allowed modifiers and values for a macro.
macroOptsWith keywords =
            do wordsep
               sepEndBy (do
                 key <- choice $ fmap (try.string) keywords
                 wordsep1
                 val <- if key == ":"
                       then many1 (noneOf "#\n\r") >>= return.MOptS
                       else if key `elem` ["from","to"]
                            then name >>= return.MOptS
                            else if key `elem` ["start","end"]
                                 then date >>= return.MOptD
                                 else number >>= return.MOptI --on
                 return (key, val)) wordsep1


applyMOpt :: Macro -> (String,MOpt) -> Macro
applyMOpt m (":", MOptS s) = m {mText=s}
applyMOpt m ("from", MOptS s) = m {mSrc=s}
applyMOpt m ("to", MOptS s) = m {mDst=s}
applyMOpt m ("start", MOptD d) = m {mStart=Just d}
applyMOpt m ("end", MOptD d) = m {mEnd=Just d}
applyMOpt m ("every", MOptI i) = m {mEvery=i}
applyMOpt m ("on", MOptI i) = m {mDay=i}
applyMOpt m _ = m -- everything else is unknown
-- |apply a list of modifiers to a macro (helper function for different keywords)
applyMOpts = flip $ foldl' applyMOpt

-- |Construct a parser for a new sugar keyword for transfers (directly translated to transfers).
-- Expects the keyword name, a base transfer with preset default values and a list of allowed
-- modifiers for this specific keyword (as you may wish to limit them for different cases)
verb vname base mods = do string vname >> wordsep1
                          val <- valOrAll
                          opts <- transferOptsWith mods
                          notFollowedBy alphaNum
                          let trans = applyTOpts opts base{tVal=val}
                          return $ AtomT trans
                          <?> vname

verb_init = verb "Init" nullTransfer{tDst="main"} []
verb_move = verb "Move" nullTransfer{tSrc="main"} transferMods
verb_in   = verb "In"   nullTransfer{tDst="main"} (transferMods\\["from"])
verb_out  = verb "Out"  nullTransfer{tSrc="main"} (transferMods\\["to"])

verb_regular = do string "Regular" >> wordsep1
                  val <- valOrAll
                  opts <- macroOptsWith macroMods
                  wordsep
                  let m = applyMOpts opts nullMacro{mVal=val,mDst="main"}
                  return $ AtomM [m]
                  <?> "Regular"

verb_save = do string "Save" >> wordsep1
               val <- value
               wordsep1
               stack <- name
               opts <- macroOptsWith $ macroMods\\["from","to"]
               let m = applyMOpts opts nullMacro{mVal=Just val,mSrc="main",mDst=stack,mText="Save "++stack}
               return $ AtomM [m]
               <?> "Save"

verb_limit = do string "Limit" >> wordsep1
                val <- value
                wordsep1
                stack <- name
                opts <- macroOptsWith $ macroMods\\["from","to"]
                wordsep
                return $ AtomM $ map (applyMOpts opts) [nullMacro{mVal=Nothing,mSrc=stack,mDst="main",mText="End Limit "++stack}
                                                       ,nullMacro{mVal=Just val,mSrc="main",mDst=stack,mText="New Limit "++stack}]
                <?> "Limit"

abc_import = do string "Import" >> wordsep1
                filename <- name
                return $ AtomI filename
                <?> "Import"

abc_auto = do string "Auto" >> wordsep1
              stack <- name
              wordsep1
              first <- many1 $ noneOf "#\n\r\t :"
              wordsep1
              rest <- sepBy name wordsep1
              let (wlist,desc) = span (/=":") (first:rest)
              return $ AtomA stack wlist $ unwords (if null desc then desc else tail desc)

---- Basic lexical helper stuff

-- | reads YYYY-MM-DD, returns a Day
date = do y <- number
          char '-'
          m <- number
          char '-'
          d <- number
          return $ fromGregorian y (fromIntegral m) (fromIntegral d)
          <?> "YYYY-MM-DD"

name = many1 (noneOf "#\n\r\t ") <?> "name"

-- |read number without sign
number = do num <- many1 digit
            return (read num :: Integer)
            <?> "number"

-- |read number with optional negative sign
integer = do s <- optionMaybe $ char '-'
             n <- number
             return $ if isNothing s then n else (-n)
             <?> "integer"

-- |read a comment starting with #
comment = wordsep >> char '#' >> many (noneOf "\n\r")

wordsep = many (oneOf " \t") <?> "whitespace"
wordsep1 = many1 (oneOf " \t") <?> "whitespace"

-- |Read a number, optionally ignore one symbol (currency)
value = do v <- integer
           optional $ noneOf "#\n\r\t "
           return v
           <?> "value"

----

-- |Merge one atom of information into a MoneyConf
mergeAtoms :: MoneyConf -> ConfAtom -> MoneyConf
mergeAtoms c AtomEmpty = c
mergeAtoms c (AtomA s w d) = c{cImportRules=(ImportRule w s d):(cImportRules c)}
mergeAtoms c (AtomI s) = c{cImportFile=Just s}
mergeAtoms c (AtomO d) = if cOrigin c == nullDate then c{cOrigin=d} else c
mergeAtoms c (AtomT t) = c{cTransfers=merge (cTransfers c) [t']}
  where t' = if cOrigin c == nullDate || tDate t /= nullDate then t else t{tDate=cOrigin c}
mergeAtoms c (AtomM ms) = if cOrigin c == nullDate
                          then c -- cannot expand macros if no origin specified previously -> ignore
                          else c{ cTransfers=foldl' merge (cTransfers c)
                                                 $ map (expandMacro (cOrigin c)) ms }


-- |The heart of the parser - read filename, a configuration file string, spit out moneystacks config
parseMoneyConf :: String -> String -> Either ParseError MoneyConf
parseMoneyConf filename input =
  case parse file filename input of
  Left e -> Left e
  Right atoms -> do
    let numOrigins = length $ filter isOrigin atoms
        numImports = length $ filter isImport atoms
        firstAtomO = (numOrigins >= 1) && (isOrigin $ head $ filter (/=AtomEmpty) atoms)
    if not firstAtomO
      then Left $ newErrorMessage (SysUnExpect "origin must be first statement in configuration!") (initialPos filename)
      else if numOrigins>1 || numImports>1
           then Left $ newErrorMessage (SysUnExpect "multiple Origins or Imports found in configuration!") (initialPos filename)
           else Right $ foldl' mergeAtoms nullMoneyConf atoms

-- |Parse and merge imported transfers from other file into given config
parseImported :: MoneyConf -> String -> Either ParseError MoneyConf
parseImported conf input = case cImportFile conf of
  Nothing -> Right conf
  Just filename -> case parse file filename input of
    Left e -> Left e
    Right atoms -> Right $ conf{cImported = cTransfers
                                         $ foldl' mergeAtoms conf{cTransfers=[]}
                                         $ filter isTransfer atoms}

-- |accept partial dates on command line and complete missing fields from other date (today's date to be supplied)
parseArgDate :: Day -> String -> Maybe Day
parseArgDate base str =
  case parse partialDate "(parseDate)" str of
       Left _ -> Nothing
       Right day -> let l = map fromIntegral $ zipWith (\a b-> if a==0 then b else a) day base'
                    in  Just $ fromGregorian (l!!0) (l!!1) (l!!2)
  where (y,m,d) = toGregorian base
        base'   = [y,fromIntegral m,fromIntegral d]

-- | reads [[YYYY-]MM-]DD, returns [year, month, day], year/month may be zero
partialDate = do
  num1 <- number
  num2 <- optionMaybe restInt
  num3 <- optionMaybe restInt
  wordsep1 <|> (eof >> return "")
  return $ reverse $ take 3 $ catMaybes [num3,num2,Just num1] ++ zero
  <?> "date"
  where restInt = char '-' >> number
        zero = 0:zero

-- | like statement/verb, but only accepts single transfers
argTransfer = do wordsep
                 t <- choice (fmap try [transfer,verb_move,verb_in,verb_out])
                 notFollowedBy alphaNum
                 return t

-- | parse a transfer passed over the command line to be appended from argument.
-- Takes a base day to be used if no date is specified (today's date)
parseArgTransfer :: Day -> String -> Either ParseError Transfer
parseArgTransfer day str =
  case parse argTransfer parseSrcName str of
    Left err -> Left err
    -- If no date is supplied on the command line, automatically use passed today's date
    Right (AtomT tr) -> Right $ if tDate tr==nullDate then tr{tDate = day} else tr
    Right _ -> Left $ newErrorUnknown $ initialPos parseSrcName -- should never happen
  where parseSrcName = "command line argument"

