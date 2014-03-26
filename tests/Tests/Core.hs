module Tests.Core where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import MoneyStacks.Core
import Data.List (sort)
import Data.Time.Calendar (fromGregorian)

coreTests = testGroup "MoneyStacks.Core" [qcProps, coreUnitTests]

qcProps = testGroup "QuickCheck Properties"
  [ testProperty "merge - keeps sorted lists sorted" testMergeSorted
  , testProperty "merge - is commutative" testMergeCommutative
  , testProperty "applyTransfer - does not lose money between stacks" testApplyTransferBetween
  , testProperty "applyTransfer - symmetric (a >(10)> b == b >(-10)> a)" testApplyTransferSymmetry
  ]

coreUnitTests = testGroup "HUnit tests"
  [ testCase "applyTransfer - testing Nothing as amount behaviour" $  [] @=? (foldl applyTransfer [] dummyTransfers1)
  , testCase "applyTransfer - testing outside world transactions"  $  [] @=? (foldl applyTransfer [] dummyTransfers2)
  , testCase "expandMacro - supply interval out of range" $ [] @=? (expandMacro nullDate nullMacro{mEvery=0})
  , testCase "expandMacro - supply day out of range" $ (expandMacro nullDate nullMacro{mDay=0}) @?= []
  , testCase "expandMacro - supply day out of range 2" $ True @=? null (expandMacro nullDate nullMacro{mDay=32})
  , testCase "expandMacro - first Transfer not before origin" $ testExpandMacroFirstDate
  , testCase "expandMacro - first Transfer not before first date" $ testExpandMacroFirstDate2
  , testCase "expandMacro - last Transfer not after end date" $ testExpandMacroLastDate
  , testCase "expandMacro - correctness of a more real-life test case" $ testExpandMacroCase1
  ]

-- QuickCheck properties:
-------------------------
instance Arbitrary Transfer where
  arbitrary = do
    val <- arbitrary
    src <- listOf1 $ elements ['a'..'z']
    dst <- listOf1 $ elements ['a'..'z']
    y <- arbitrary
    m <- arbitrary
    d <- arbitrary
    let date = fromGregorian y m d
    return $ Transfer val src dst date "Arbitrary transfer"

testMergeSorted :: [Integer] -> [Integer] -> Bool
testMergeSorted a b = sort merged == merged
  where merged = merge a' b'
        a' = sort a
        b' = sort b

testMergeCommutative :: [Integer] -> [Integer] -> Bool
testMergeCommutative a b = merge a' b' == merge b' a'
  where a' = sort a
        b' = sort b

-- |Test whether transfers between stacks preserve value / transfers without stack lose value
-- This test checks whether the total value of all stacks changes after in-stack transfers
-- the stack list may not contain a stack named "" (this is used as temporary stack for outside
-- transfers)
testApplyTransferBetween :: [Stack] -> Transfer -> Bool
testApplyTransferBetween s t = not (tSrc t=="" && tDst t=="" || tSrc t/="" && tDst t/="")
                               || total s' == total ( applyTransfer s' t )
  where total = sum . map snd
        s' = filter ((/="").fst) s -- empty stackname forbidden

testApplyTransferSymmetry s t =
  applyTransfer s' t == applyTransfer s' t{tSrc=tDst t,tDst=tSrc t,tVal=fmap negate $ tVal t}
  where s' = filter ((/="").fst) s -- empty stackname forbidden

-- Core HUnit tests:
--------------------

dummyTransfers1 = --testing behaviors of Nothing value
  [ nullTransfer { tVal=Just 100, tSrc="main", tDst="test"}
  , nullTransfer { tVal=Nothing, tSrc="test", tDst="main"}
  , nullTransfer { tVal=Just 10, tSrc="", tDst="main"}
  , nullTransfer { tVal=Nothing, tSrc="main", tDst=""}
  , nullTransfer { tVal=Nothing, tSrc="", tDst=""}
  , nullTransfer { tVal=Nothing, tSrc="", tDst="main"}
  ]
dummyTransfers2 = --testing behaviours of values and outside world interactions
  [ nullTransfer
  , nullTransfer { tVal=Just 50, tSrc="main", tDst=""}
  , nullTransfer { tVal=Just (-50), tSrc="main", tDst=""}
  , nullTransfer { tVal=Just 50, tSrc="", tDst="main"}
  , nullTransfer { tVal=Just (-50), tSrc="", tDst="main"}
  ]

testExpandMacroFirstDate = s `compare` (tDate $ head $ expandMacro s m) @?= LT
  where s = fromGregorian 2014 01 02
        m = Macro (Just 10) "bla" "blub" 1 Nothing Nothing 1 "test macro"

testExpandMacroFirstDate2 = s `compare` (tDate $ head $ expandMacro s m) @?= LT
  where s = fromGregorian 2014 01 13
        m = Macro (Just 10) "bla" "blub" 1 (Just s) Nothing 1 "test macro"

testExpandMacroLastDate = (tDate $ last $ expandMacro s m) `compare` e @?= LT
  where s = fromGregorian 2014 01 13
        e = fromGregorian 2014 05 02
        m = Macro (Just 10) "bla" "blub" 13 Nothing (Just e) 2 "test macro"

testExpandMacroCase1 =
  applyUntil (fromGregorian 2014 08 01) (expandMacro nullDate
  Macro {mVal=Just 375, mSrc="",mDst="main", mDay=3, mStart=Just $ fromGregorian 2014 04 5,
  mEnd=Just $ fromGregorian 2014 07 31, mEvery=2, mText="Job"}) @?=[( "main",750 )]
