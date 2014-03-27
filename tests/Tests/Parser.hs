module Tests.Parser where
import Test.Tasty
import Test.Tasty.HUnit

import MoneyStacks.Core
import MoneyStacks.Parser

import Data.Either
import Data.Maybe (mapMaybe)
import Data.Time.Calendar (fromGregorian)

-- stubTest str = testCase str $ assertFailure "test not written!"

parserTests = testGroup "MoneyStacks.Parser" [parserUnitTests]

parserUnitTests = testGroup "HUnit tests" [ testParseArgDateFail
                                          , testParseArgDateSuccess
                                          , testParseArgTransferFail
                                          , testParseArgTransferSuccess
                                          , testParseConfFail
                                          , testParseConfSuccess
                                          ]

testParseArgDateFail = testCase "parseDate - test some invalid date strings"
  $ [] @=? (mapMaybe (\str -> parseArgDate nullDate str)
                     ["blub1", "-1-", "1-", "1-2-", "1-2-3-", "1-2-3bla"])

testParseArgDateSuccess = testCase "parseDate - test some valid partial date strings"
  $ correct @=? (mapMaybe (\str -> parseArgDate baseDate str)
                          [ "1", "1-2", "1-2-3" ])
  where baseDate = fromGregorian 2014 6 2
        correct = [fromGregorian 2014 6 1, fromGregorian 2014 1 2, fromGregorian 1 2 3]

testParseArgTransferFail = testCase "argTransfer - test some invalid transfer arguments"
  $ [] @=? (rights $ map (\str -> parseArgTransfer nullDate str)
                         [ "Transfer", "Move", "In", "Out", "Transfer bla", "Move 10 blub"
                         , "Move from main", "In 100 on 1", "In 100 from main", "Out 100Eur"
                         , "Out 100 to main", "Move everything from main to blub blah"
                         ])

testParseArgTransferSuccess = testCase "argTransfer - test some valid transfer arguments"
  $ [] @=? (map getParseErrors $ lefts $ map (\str -> parseArgTransfer baseDate str)
                         [ "Out 100€ from House", "In 50$ : Some income"
                         , "Move everything from main to blub #just a commment"
                         , "Out 100 on 2014-01-01", "In 20 to misc #ignored comment"
                         ])
  where baseDate = fromGregorian 2014 6 2

testParseConfFail = testCase "parseMoneyConf - test some invalid inputs"
  $ [] @=? (rights $ map (\str -> parseMoneyConf "(test input)" str)
                         [ "Origin 2001-02-03\n  bla","blub","Origin", "Move", "Regular #blub" -- syntax errors
                         , "Origin 2001-02-03\nMove #invalid line"
                         , "Origin 2001-02-03\nOrigin 2002-03-04 #multiple origins"
                         , "Move 100\nOrigin 2001-02-03 #origin after action"
                         ])

testParseConfSuccess = testCase "parseMoneyConf - test valid inputs with whitespace"
  $ [] @=? (map getParseErrors $ lefts $ map (\str -> parseMoneyConf "(test input)" str)
                         [ " \t\n #blub\n #empty line with comment\n#another comment\nOrigin 2014-01-01"
                         , " Origin 2014-01-01 #start date\nInit 100€\n\tMove everything  to\tmain\t #lol"
                         ])
