{-# LANGUAGE NoMonomorphismRestriction #-}
module MoneyStacks.Parser where
import qualified System.IO.Unsafe as Unsafe
import qualified System.IO as IO
import Text.Parsec
import Text.Parsec.Error
import Data.Time.Calendar
import Data.Time.Clock
import Data.Maybe (isNothing, catMaybes, fromJust)
import Data.List ((\\),find)

import MoneyStacks.Core

-- Each line contains an information atom
data ConfAtom = AtomT Transfer | AtomM [Macro] | AtomO Day | AtomEmpty
                deriving (Show,Eq)
isOrigin (AtomO d) = True
isOrigin _ = False

file = do atoms <- sepEndBy line newline
          notFollowedBy alphaNum
          return atoms

-- Empty lines are empty atoms, comments are ignored
line = try emptyline <|> do
            s <- statement
            optional comment
            return s

emptyline = do
  try comment <|> do
              wordsep
              notFollowedBy alphaNum
              return ""
  return AtomEmpty <?> "whitespace"

-- A statement begins with a basic or complex verb (keyword), indentation does not matter
statement = wordsep >> choice (fmap try [origin,transfer,macro
                             ,verb_init,verb_move,verb_in,verb_out
                             ,verb_regular,verb_save,verb_limit])

origin = do string "Origin"
            wordsep1
            d <- date
            return $ AtomO d
            <?> "Origin"

optVerbVal = option (Just 0) $ choice [try $ string "everything" >> return Nothing, value >>= return.Just]

transfer = do string "Transfer"
              wordsep1
              val <- optVerbVal
              opts <- transferOptsWith transferMods
              let trans = foldl applyTOpt nullTransfer{tVal=val} opts
              return $ AtomT trans
              <?> "Transfer"

transferMods = ["from","on","to",":"]

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

macro = do string "Macro"
           wordsep1
           val <- optVerbVal
           opts <- macroOptsWith macroMods
           let m = foldl applyMOpt nullMacro{mVal=val} opts
           return $ AtomM [m]
           <?> "Macro"

data MOpt = MOptS String | MOptI Integer | MOptD Day
macroMods = ["from","to","on","start","end","every",":"]

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

---- Syntactic sugar verbs (directly translated to transfers/macros)
verb name base mods = do string name >> wordsep1
                         val <- value
                         opts <- transferOptsWith mods
                         let trans = foldl applyTOpt base{tVal=Just val} opts
                         return $ AtomT trans
                         <?> name

verb_init = verb "Init" nullTransfer{tDst="main"} []
verb_move = verb "Move" nullTransfer{tSrc="main"} transferMods
verb_in   = verb "In"   nullTransfer{tDst="main"} (transferMods\\["from"])
verb_out  = verb "Out"  nullTransfer{tSrc="main"} (transferMods\\["to"])

verb_regular = do string "Regular" >> wordsep1
                  val <- value
                  opts <- macroOptsWith macroMods
                  wordsep
                  let m = foldl applyMOpt nullMacro{mVal=Just val,mDst="main"} opts
                  return $ AtomM [m]
                  <?> "Regular"

verb_save = do string "Save" >> wordsep1
               val <- value
               wordsep1
               stack <- name
               opts <- macroOptsWith $ macroMods\\["from","to"]
               let m = foldl applyMOpt nullMacro{mVal=Just val,mSrc="main",mDst=stack,mText="Save "++stack} opts
               return $ AtomM [m]
               <?> "Save"

verb_limit = do string "Limit" >> wordsep1
                val <- value
                wordsep1
                stack <- name
                opts <- macroOptsWith $ macroMods\\["from","to"]
                wordsep
                let rem = foldl applyMOpt nullMacro{mVal=Nothing,mSrc=stack,mDst="main",mText="End Limit "++stack} opts
                    add = foldl applyMOpt nullMacro{mVal=Just val,mSrc="main",mDst=stack,mText="New Limit "++stack} opts
                return $ AtomM[rem, add]
                <?> "Limit"

---- Basic lexical helper stuff

-- YYYY-MM-DD -> Day
date = do y <- number
          char '-'
          m <- number
          char '-'
          d <- number
          return $ fromGregorian y (fromIntegral m) (fromIntegral d)
          <?> "YYYY-MM-DD"

name = many1 (noneOf "#\n\r\t ") <?> "name"

-- read number without sign
number = do num <- many1 digit
            return (read num :: Integer)
            <?> "number"

-- read number with optional negative sign
integer = do s <- optionMaybe $ char '-'
             n <- number
             return $ if isNothing s then n else (-n)
             <?> "integer"

comment = wordsep >> char '#' >> many (noneOf "\n\r")

wordsep = many (oneOf " \t") <?> "whitespace"
wordsep1 = many1 (oneOf " \t") <?> "whitespace"

-- Read a number, ignore one symbol (currency)
value = do v <- integer
           optional $ noneOf "\n\r \t"
           return v
           <?> "value"

----

-- Each atom contains information to be merged into the final conf
mergeAtoms :: MoneyConf -> ConfAtom -> MoneyConf
mergeAtoms c AtomEmpty = c
mergeAtoms c (AtomO d) = c{cOrigin=d}
mergeAtoms c (AtomT t) = c{cTransfers=merge (cTransfers c) [t']}
  where t' = if cOrigin c == nullDate || tDate t /= nullDate then t else t{tDate=cOrigin c}
mergeAtoms c (AtomM ms) = if cOrigin c == nullDate
                          then c -- cannot expand macros if no origin specified previously->discard
                          else c{ cTransfers=foldl merge (cTransfers c)
                                                 $ map (expandMacro (cOrigin c)) ms }


-- The heart of the parser - read a string, spit out moneystacks config
parseMoneyConf :: String -> String -> Either ParseError MoneyConf
parseMoneyConf filename input = do
    atoms <- parse file filename input
    return $ foldl mergeAtoms nullMoneyConf atoms

-- accept partial dates on command line and complete missing fields from other date (today's date supplied)
parseDate base str = case parse partialDate "(parseDate)" str of
                     Left _ -> Nothing
                     Right d -> let l = map fromIntegral $ zipWith (\x y-> if x==0 then y else x) d base'
                                  in Just $ fromGregorian (l!!0) (fromIntegral (l!!1)) (fromIntegral (l!!2))
  where (y,m,d) = toGregorian base
        base'   = [y,fromIntegral m,fromIntegral d]

-- returns [year, month, day], year/month may be zero
partialDate = do
  num1 <- number
  num2 <- optionMaybe restInt
  num3 <- optionMaybe restInt
  return $ reverse $ take 3 $ catMaybes [num3,num2,Just num1] ++ zero
  <?> "date"
  where restInt = char '-' >> number
        zero = 0:zero

-- like statement/verb
parseArgTransfer = do wordsep
                      choice $ fmap try [transfer,verb_move,verb_in,verb_out]

-- parse a transfer to be appended from argument
argTransfer :: Day -> String -> Either ParseError Transfer
argTransfer day str = case parse parseArgTransfer "(command line argument)" str of
                  Left err -> Left err
                  Right (AtomT tr) -> Right tr{tDate = day}

-- generate a sane error message
getParseErrors = (showErrorMessages "or" "unknown" "expecting" "unexpected" "end of input").errorMessages
prettyParseError err = "parse error in " ++ (show $ errorPos err) ++ ": " ++ getParseErrors err
