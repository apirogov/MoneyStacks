{-|
Module      : MoneyStacks.Core
Description : Data structures and combinators for MoneyStacks
Copyright   : (c) Anton Pirogov, 2014
License     : MIT
Stability   : experimental
Portability : POSIX
-}
module MoneyStacks.Core
where
import Data.Maybe (fromMaybe,isNothing)
import Data.Function (on)
import qualified Data.Map as M
import Data.Time.Calendar
import Data.List (foldl')

-- |MoneyConf is the structure generated from a MoneyStacks configuration file by the parser
data MoneyConf = MoneyConf { cOrigin :: Day -- ^This is the day where the macros get expanded from and also default fallback day
                           , cTransfers :: [Transfer] -- ^This is the (infinite) chronological list of transfers collected by the parser
                           , cImportFile :: Maybe String -- ^This is the file for automatic banking imports
                           , cImportRules :: [ImportRule] -- ^List of rules to assign stacks and descriptions on CSV import
                           , cImported :: [Transfer] -- ^This is the list of imported transfers
                           } deriving (Show,Eq)

-- |A single instance of a word match rule to be applied when importing CSV files
data ImportRule = ImportRule { rWords :: [String] -- ^Words to be looked for (one is enough for match)
                             , rStack :: StackName -- ^Name of stack to be assigned
                             , rAddTxt :: String  -- ^Text appended to description
                             } deriving (Show,Eq)

type StackName = String -- ^A stack name is just a string
type Stack = (StackName, Integer) -- ^Must not contain a stack with empty StackName (used internally)

-- |A Transfer is an atomic value transaction between stacks or a stack with the outside world
-- Money is coming from / going to the outside world (leaving the system)
-- if the source or destination stack name is omitted.
data Transfer = Transfer { tVal :: Maybe Integer -- ^if Nothing, the complete source stack amount will be transferred (for macros)
                         , tSrc :: StackName
                         , tDst :: StackName
                         , tDate :: Day          -- ^Date of transaction
                         , tText :: String       -- ^Description of the transfer as to be shown in the human readable log
                         } deriving (Eq)
instance Ord Transfer where
  compare = compare `on` tDate
instance Show Transfer where
  show = showTransfer False

-- |Generates both the syntactically correct config file syntax for a transfer
-- and the pretty printed output for the log (the date up front)
showTransfer forLog (Transfer v s d dat t)
  | v' < 0 =showTransfer forLog (Transfer (Just (-v')) d s dat t)
  | forLog = show dat ++ " -> " ++ verb ++ amount ++ from ++ to ++ text -- for human (date first)
  | otherwise = verb ++ amount ++ from ++ to ++ date ++ text -- for parser (correct syntax)
  where v' = fromMaybe 0 v
        verb = if null s == null d then "Move " else if null s then "In " else if null d then "Out " else "Move "
        amount = if isNothing v then "everything" else (show v')
        from   = if null s || s=="main" then "" else " from " ++ ticks s
        to     = if null d || d=="main" then "" else " to " ++ ticks d
        date   = if nullDate==dat then "" else " on " ++ show dat
        text   = if null t then "" else " : " ++ t
        ticks str  = if forLog then show str else str -- if for human log, make str -> "str" for pretty printing


-- |A Macro is a recurring Transfer spawner to be expanded to a list of Transfers (e.g. regular income or spendings)
data Macro = Macro { mVal  :: Maybe Integer
                   , mSrc :: StackName
                   , mDst :: StackName
                   , mDay  :: Integer    -- ^Day of Month (1-31)
                   , mStart :: Maybe Day -- ^optional start date (day ignored). If unset, Origin is used in expansion
                   , mEnd :: Maybe Day   -- ^optional end date. If unset, this macro has no end
                   , mEvery :: Integer   -- ^Number of months (interval between transactions)
                   , mText :: String     -- ^Description text for each transfer spawned from this macro
                   } deriving (Show, Eq)

-- helper dummies
-- |Default date (0000-01-01), used as a null value
nullDate = fromGregorian 0 0 0
-- |Transfer constructor, defaults to null values (no stack names, nullDate, no description and amount of 0)
nullTransfer = Transfer{tVal=Just 0, tSrc="", tDst="", tDate=nullDate, tText=""}
-- |Macro constructor, like transfer constructor defaults to null values, monthly interval (every 1)
-- on the 1st day of the month and no specified start or end date (expanded from origin to infinity)
nullMacro = Macro{mVal=Just 0,mSrc="",mDst="",mDay=1,mStart=Nothing,mEnd=Nothing,mEvery=1,mText=""}
-- |MoneyConf constructor, defaults the origin as null date and an empty transfer list
nullMoneyConf = MoneyConf{cOrigin=nullDate,cTransfers=[],cImportFile=Nothing,cImportRules=[],cImported=[]}

-- |Generate an list of Transfers from a Macro (needs the origin date as fallback start date)
-- Does not return transfers before origin, returns empty list with invalid input
-- generates from given start or origin to given end or infinity
expandMacro :: Day -> Macro -> [Transfer]
expandMacro s m
 | mEvery m < 1 || mDay m < 1 || mDay m > 31 = [] -- invalid arguments
 | otherwise = dropWhile beforeBeginning . takeWhile notAfterEnd
             $ iterate (incTrans $ mEvery m) firstTrans
      where beforeBeginning curr = tDate curr < s
            notAfterEnd curr = tDate curr <= (fromMaybe tomorrow $ mEnd m)
              where tomorrow = addDays 1 $ tDate curr
            applyDay date = fromGregorian year month where (year,month,_) = toGregorian date
            incTrans n t  = t {tDate=addGregorianMonthsClip n $ tDate t}
            firstTrans = Transfer {
                  tVal  = mVal m, tSrc = mSrc m ,tDst = mDst m, tText = mText m
                , tDate = applyDay (fromMaybe s $ mStart m) $ fromIntegral $ mDay m
                }

-- |Merge together sorted lists into one sorted list (Macro expansions and single transfers to one stream)
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) = if x<=y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

-- |Apply a single transfer to a state of stacks, only keep named non-empty stacks
applyTransfer :: [Stack] -> Transfer -> [Stack]
applyTransfer s t = M.toList . M.filter (/= 0) . M.delete "" $ moveMoney s'
  where s' = M.fromList s
        moveMoney = M.insertWith (+) (tSrc t) (-val) . M.insertWith (+) (tDst t) val
        val = fromMaybe sourceVal $ tVal t
        sourceVal = fromMaybe 0 (M.lookup (tSrc t) s')

-- |Apply an (infinite) list of transfers until a given day and return stacks
applyUntil :: Day -> [Transfer] -> [Stack]
applyUntil d t = foldl' applyTransfer [] $ takeWhile (\x -> tDate x <= d) t

-- Same as applyUntil, but returns all intermediate results. Might be useful for debugging:
-- applyUntil' d t = scanl applyTransfer [] $ takeWhile (\x -> tDate x <= d) t

--------------------------------
-- MoneyConf interface functions

-- |Wrapper around applyUntil to use the MoneyConf built by the parser
calcStacksUntil :: MoneyConf -> Day -> [Stack]
calcStacksUntil c d = applyUntil d $ merge (cTransfers c) (cImported c)

-- |Generate human-readable transfer logs for a given config and date range (inclusive)
showTransfersFromTo :: MoneyConf -> Day -> Day -> [String]
showTransfersFromTo c s d = map (showTransfer True) $ dropWhile (\x -> tDate x < s)
                                            $ takeWhile (\x -> tDate x <= d)
                                            $ merge (cTransfers c) (cImported c)

