module Main where

-- Put your black-box tests in this file

import Defs
import Parser (parseDatabase)
import Solver (install)

import Test.Tasty
import Test.Tasty.HUnit


tests = testGroup "Unit Tests" [
    utilities,
    predefined
    ]

utilities = testGroup "Utilities tests" [
        testCase "Version 1 <= 2" $ V [VN 1 ""] <= V [VN 2 ""] @?= True,
        testCase "Version 2 <= 1" $ V [VN 2 ""] <= V [VN 1 ""] @?= False,
        testCase "Version 1a <= Verion1z" $ V [VN 1 "a"] <= V [VN 1 "z"] @?= True,
        testCase "Version 1.1 <= 1.2" $ V [VN 1 "", VN 1 ""] <= V [VN 1 "", VN 2 ""] @?= True,
        testCase "Version 1.2 <= 1.1" $ V [VN 1 "", VN 2 ""] <= V [VN 1 "", VN 1 ""] @?= False,
        testCase "Version 1.1a <= 1.1b" $ V [VN 1 "", VN 1 "a"] <= V [VN 1 "", VN 1 "b"] @?= True,
        testCase "Version 4.0.1 <= 04.00.001" $ V [VN 4 "", VN 0 "", VN 1 ""] <= V [VN 04 "", VN 00 "", VN 001 ""] @?= True,
        testCase "Version 4.0.1.3 <= 4.1.2" $ V [VN 4 "", VN 0 "", VN 1 "", VN 3 ""] <= V [VN 4 "", VN 1 "", VN 2 ""] @?= True,
        testCase "802.11 <= 802.11n" $ V [VN 8 "", VN 0 "", VN 2 "", VN 1 "", VN 1 ""] <= V [VN 8 "", VN 0 "", VN 2 "", VN 1 "", VN 1 "n"] @?= True,
        testCase "802.11n <= 802.11ax" $ V [VN 8 "", VN 0 "", VN 2 "", VN 1 "", VN 1 "n"] <= V [VN 8 "", VN 0 "", VN 2 "", VN 1 "", VN 1 "ax"] @?= True,
        testCase "802.11ax <= 802.11bb" $ V [VN 8 "", VN 0 "", VN 2 "", VN 1 "", VN 1 "ax"] <= V [VN 8 "", VN 0 "", VN 2 "", VN 1 "", VN 1 "bb"] @?= True
    ]

-- just a sample; feel free to replace with your own structure
predefined = testGroup "Unit tests"
  [testGroup "Parser tests"
     [testCase "tiny" $
        parseDatabase "package {name foo}" @?= Right db],
   testGroup "Solver tests"
     [testCase "tiny" $
        install db pname @?= Just [(pname, ver)] ] ]
  where
    pname = P "foo"
    ver = V [VN 1 ""]
    db = DB [Pkg pname ver "" []]

main = defaultMain tests

