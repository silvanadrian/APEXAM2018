module Main where

-- Put your black-box tests in this file

import Defs
import Utils
import Parser (parseDatabase)
import Solver (install)

import Test.Tasty
import Test.Tasty.HUnit


tests = testGroup "Unit Tests"
    [
        utilities
        --predefined
    ]

utilities = testGroup "Utilities tests"
    [
        -- Versions
        testCase "Version 1 <= 1" $ V [VN 1 ""] <= V [VN 1 ""] @?= True,
        testCase "Version 1 <= 2" $
            V [VN 1 ""] <= V [VN 2 ""] @?= True,
        testCase "Version 2 <= 1" $
            V [VN 2 ""] <= V [VN 1 ""] @?= False,
        testCase "Version 1a <= Verion1z" $
            V [VN 1 "a"] <= V [VN 1 "z"] @?= True,
        testCase "Version 1.1 <= 1.2" $
            V [VN 1 "", VN 1 ""] <= V [VN 1 "", VN 2 ""] @?= True,
        testCase "Version 1.2 <= 1.1" $
            V [VN 1 "", VN 2 ""] <= V [VN 1 "", VN 1 ""] @?= False,
        testCase "Version 1.1a <= 1.1b" $
            V [VN 1 "", VN 1 "a"] <= V [VN 1 "", VN 1 "b"] @?= True,
        testCase "Version 4.0.1 <= 04.00.001" $
            V [VN 4 "", VN 0 "", VN 1 ""] <= V [VN 04 "", VN 00 "", VN 001 ""] @?= True,
        testCase "Version 4.0.1.3 <= 4.1.2" $
            V [VN 4 "", VN 0 "", VN 1 "", VN 3 ""] <= V [VN 4 "", VN 1 "", VN 2 ""] @?= True,
        testCase "802.11 <= 802.11n" $ V [VN 802 "", VN 11 ""] <= V [VN 802 "", VN 11 "n"] @?= True,
        testCase "802.11n <= 802.11ax" $ V [VN 802 "", VN 11 "n"] <= V [VN 802 "", VN 11 "ax"] @?= True,
        testCase "802.11ax <= 802.11bb" $ V [VN 802 "", VN 11 "ax"] <= V [VN 802 "", VN 11 "bb"] @?= True,
        -- Merge Constraints
        testCase "Merge 2 Empty Lists" $ merge [] [] @?= Just [],
        testCase "Merge non empty and empty List" $ merge
            [(P "Test", (False, V [VN 0 ""] , V [VN 1 ""] ))] [] @?=
            Just [(P "Test", (False, V [VN 0 ""] , V [VN 1 ""] ))],
        testCase "Merge 2 non empty Lists" $ merge
            [(P "Test", (False, V [VN 0 ""] , V [VN 1 ""] ))] [(P "Test", (False, V [VN 0 ""] , V [VN 1 ""] ))] @?=
            Just [(P "Test",(False,V [VN 0 ""],V [VN 1 ""]))],
        testCase "Merge True and False" $ merge
            [(P "Test", (True, V [VN 2 ""] , V [VN 8 ""] ))] [(P "Test", (False, V [VN 4 ""] , V [VN 6 ""] ))] @?=
            Just [(P "Test",(True,V [VN 4 ""],V [VN 6 ""]))],
        testCase "Merge True and False 2nd example" $ merge
            [(P "Test", (True, V [VN 4 ""] , V [VN 6 ""] ))] [(P "Test", (False, V [VN 3 ""] , V [VN 8 ""] ))] @?=
            Just [(P "Test",(True,V [VN 4 ""],V [VN 6 ""]))],
        testCase "Merge False and False" $ merge
            [(P "Test", (False, V [VN 4 ""] , V [VN 6 ""] ))] [(P "Test", (False, V [VN 3 ""] , V [VN 8 ""] ))] @?=
            Just [(P "Test",(False,V [VN 4 ""],V [VN 6 ""]))],
        testCase "Merge False and True" $ merge
            [(P "Test", (False, V [VN 4 ""] , V [VN 6 ""] ))] [(P "Test", (True, V [VN 3 ""] , V [VN 8 ""] ))] @?=
            Just [(P "Test",(True,V [VN 4 ""],V [VN 6 ""]))],
        testCase "Merge Many Constrints example" $ merge
            [(P "Test", (True, V [VN 4 ""] , V [VN 6 ""] )),  (P "Test2", (False, V [VN 3 "a"], V [VN 9 ""])),
             (P "Test3", (False, V [VN 1 ""], V [VN 10 ""]))]
            [(P "Test", (False, V [VN 3 ""] , V [VN 8 ""] )), (P "Test2", (False, V [VN 3 "z"], V [VN 7 ""]))] @?=
            Just [(P "Test",(True,V [VN 4 ""],V [VN 6 ""])),(P "Test2",(False,V [VN 3 "z"],V [VN 7 ""]))],
        testCase "Merge same Version" $ merge
                    [(P "Test", (False, V [VN 1 ""] , V [VN 1 ""] ))] [] @?=
                    Just [(P "Test", (False, V [VN 1 ""] , V [VN 1 ""] ))]
    ]



-- just a sample; feel free to replace with your own structure
predefined = testGroup "predefined"
  [testGroup "Parser tests"
     [testCase "tiny" $
        parseDatabase "package {name foo}package {name foo}package {name foo}" @?= Right db],
   testGroup "Solver tests"
     [testCase "tiny" $
        install db pname @?= Just [(pname, ver)] ] ]
  where
    pname = P "foo"
    ver = V [VN 1 ""]
    db = DB [Pkg pname ver "" []]

main = defaultMain tests

