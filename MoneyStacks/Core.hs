module MoneyStacks.Core where
import Data.Maybe (fromMaybe,isNothing)
import Data.Function (on)
import Data.List (find, sort)
import Data.Time.Calendar

-- MoneyConf is the structure generated from a MoneyStacks configuration file by the parser
data MoneyConf = MoneyConf { cOrigin :: Day
                           , cTransfers :: [Transfer]
                           } deriving (Show,Eq)

-- These are some aliases for the simple money stack structure (named values)
type StackName = String
type Stack = (StackName, Integer)

-- A Transfer is an atomic value transaction between stacks or a stack with the outside world
data Transfer = Transfer { tVal :: Maybe Integer --Nothing = complete source stack amount
                         , tSrc :: StackName     --empty StackName = 'outside the system'
                         , tDst :: StackName
                         , tDate :: Day
                         , tText :: String
                         } deriving (Eq)
instance Ord Transfer where
  compare = compare `on` tDate
instance Show Transfer where
  show = showTransfer False

showTransfer forLog (Transfer v s d dat t) = if v' < 0
                              then showTransfer forLog (Transfer (Just (-v')) d s dat t)
                              else if forLog
                                   then show dat ++ " -> " ++ verb ++ amount ++ from ++ to ++ text -- for human (date first)
                                   else verb ++ amount ++ from ++ to ++ date ++ text -- for parser (correct syntax)
  where v' = fromMaybe 0 v
        verb = if null s == null d then "Move " else if null s then "In " else if null d then "Out " else "Move "
        amount = if isNothing v then "everything " else (show v')
        from   = if null s || s=="main" then "" else " from "++inTicks s
        to     = if null d || d=="main" then "" else " to " ++inTicks d
        date   = if nullDate==dat then "" else " on "++show dat
        text   = if null t then "" else " : " ++ t
        inTicks s = if forLog then show s else s -- if for human log, make str -> "str" for pretty printing


-- A Macro is a recurring Transfer spawner entity to be expanded to a list of Transfers
data Macro = Macro { mVal  :: Maybe Integer
                   , mSrc :: StackName
                   , mDst :: StackName
                   , mDay  :: Integer -- Day of Month
                   , mStart :: Maybe Day -- optional range
                   , mEnd :: Maybe Day
                   , mEvery :: Integer -- Months interval
                   , mText :: String
                   } deriving (Show, Eq)

-- helper dummies
nullDate = fromGregorian 0 0 0
nullTransfer = Transfer{tVal=Just 0, tSrc="", tDst="", tDate=nullDate, tText=""}
nullMacro = Macro{mVal=Just 0,mSrc="",mDst="",mDay=1,mStart=Nothing,mEnd=Nothing,mEvery=1,mText=""}
nullMoneyConf = MoneyConf{cOrigin=nullDate,cTransfers=[]}

-- Generate an list of Transfers from a Macro (needs the origin date as fallback start date)
-- Does not return transfers before origin, returns empty list with invalid input
-- generates from given start or origin to given end or infinity
expandMacro :: Day -> Macro -> [Transfer]
expandMacro s m
 | mEvery m <= 0 || mDay m < 1 || mDay m > 31 = [] -- invalid arguments
 | otherwise = dropWhile beforeBeginning . takeWhile notAfterEnd
             $ iterate (incTrans $ mEvery m) firstTrans
      where beforeBeginning curr = tDate curr < s
            notAfterEnd curr = tDate curr <= (fromMaybe tomorrow $ mEnd m)
              where tomorrow = addDays 1 $ tDate curr
            applyDay date = fromGregorian y m where (y,m,_) = toGregorian date
            incTrans n t  = t {tDate=addGregorianMonthsClip n $ tDate t}
            firstTrans = Transfer {
                  tVal   = mVal m
                , tSrc   = mSrc m
                , tDst   = mDst m
                , tDate  = applyDay (fromMaybe s $ mStart m) $ fromIntegral $ mDay m
                , tText  = mText m
                }

-- Merge together sorted lists into one sorted list (-> Macro expansions to one stream)
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) = if x<=y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

-- Apply a single transfer to a state of stacks, only keep named non-empty stacks
applyTransfer :: [Stack] -> Transfer -> [Stack]
applyTransfer s t = sort . filter ((/= 0) . snd) . filter ((/= "") . fst)
                  $ dst':src':(filter (not . isOldStack) s)
  where getStackBy field = fromMaybe (field t,0) $ find (\x -> fst x == field t) s
        addToStack st amount = (fst st, snd st + amount)
        src = getStackBy tSrc
        dst = getStackBy tDst
        isOldStack x = x==src || x==dst
        val = fromMaybe (snd src) $ tVal t
        src' = addToStack src (-val)
        dst' = addToStack dst val

-- Apply an (infinite) list of transfers until a given day and return stacks
applyUntil :: Day -> [Transfer] -> [Stack]
applyUntil d t = foldl applyTransfer [] $ takeWhile (\x -> tDate x <= d) t

--------------------------------
-- MoneyConf interface functions

-- Wrapper around applyUntil to use the MoneyConf built by the parser
calcStacksUntil :: MoneyConf -> Day -> [Stack]
calcStacksUntil c d = applyUntil d (cTransfers c)

-- Generate human-readable transfer logs
showTransfersFromTo :: MoneyConf -> Maybe Day -> Day -> [String]
showTransfersFromTo c Nothing d = map (showTransfer True) $ takeWhile (\x -> tDate x <= d) $ cTransfers c
showTransfersFromTo c (Just s) d = map (showTransfer True) $ dropWhile (\x -> tDate x < s)
                                            $ takeWhile (\x -> tDate x <= d) $ cTransfers c

------------------------------
-- TESTING (TODO: QuickCheck?)

-- Same as applyUntil, but returns all intermediate results

-- applyUntil' d t = scanl applyTransfer [] $ takeWhile (\x -> tDate x <= d) t

-- generate human readable log for each applied transfer

-- generateLog d t = putStrLn $ unlines
--                 $ zipWith (\a b -> (show $ tDate a) ++ ": " ++ tText a ++ " - " ++ b) t
--                 $ zipWith3 (\a b c -> show a ++ b ++ show c) stacks (repeat " -> ") (tail stacks)
--   where stacks = applyUntil' d t

-- Testing some normal and special cases.
-- try applyUntil' (should return empty stack list back)

-- defStartDate = fromGregorian 2014 01 01
-- dummyTransfer = Transfer { tVal=Just 0, tSrc="", tDst="", tDate=defStartDate, tText=""}
-- transfers = [ dummyTransfer { tVal=Just 100, tSrc="main", tDst="test"}
--   , dummyTransfer
--   , dummyTransfer { tVal=Nothing, tSrc="", tDst=""}
--   , dummyTransfer { tVal=Nothing, tSrc="", tDst="main"}
--   , dummyTransfer { tVal=Nothing, tSrc="test", tDst="main"}
--   , dummyTransfer { tVal=Just 50, tSrc="main", tDst=""}
--   , dummyTransfer { tVal=Just (-50), tSrc="main", tDst=""}
--   , dummyTransfer { tVal=Just 50, tSrc="", tDst="main"}
--   , dummyTransfer { tVal=Just (-50), tSrc="", tDst="main"}
--   , dummyTransfer { tVal=Just 10, tSrc="", tDst="main"}
--   , dummyTransfer { tVal=Nothing, tSrc="main", tDst=""}
--   ]

-- Testing macro expansion (try generateLog)

-- income = Macro {mVal=Just 375,mSrc="",mDst="main",mDay=3,
--   mStart=Just $ fromGregorian 2014 04 1, mEnd=Just $ fromGregorian 2014 07 31
--   ,mEvery=1,mText="Job Lohn"}
-- mummoney = Macro {mVal=Just 300,mSrc="",mDst="main",mDay=15,
--   mStart=Nothing, mEnd=Nothing,mEvery=1,mText="Mum Geld"}
-- living = Macro {mVal=Just 217,mSrc="main",mDst="",mDay=1,
--   mStart=Nothing, mEnd=Nothing,mEvery=1,mText="Wohnung kosten"}
-- phone = Macro {mVal=Just (-40),mSrc="",mDst="main",mDay=3,
--   mStart=Nothing, mEnd=Nothing,mEvery=3,mText="Handy Kosten"}
-- savegifts = Macro {mVal=Just 50,mSrc="main",mDst="gifts",mDay=1,
--   mStart=Nothing, mEnd=Nothing,mEvery=1,mText="Save for Gifts"}
-- transfers2 = foldl merge [] $ map (expandMacro defStartDate) [income,mummoney,living,phone,savegifts]
