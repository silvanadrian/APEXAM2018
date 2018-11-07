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
        utilities,
        parser
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


parser = testGroup "parser"
    [
        testCase "parse 3 packages with names" $
                parseDatabase "package {name foo}package {name foo}package {name foo}" @?=
                Right db1,
        testCase "parse package with name and description" $
                 parseDatabase "package {name foo;description test}" @?=
                 Right db2,
        testCase "parse package with name and description" $
                 parseDatabase "package {name foo;description test}" @?=
                 Right db2,
        testCase "parse package with name, description, version" $
                parseDatabase "package {name foo; version 1.2; description test}" @?=
                Right db3,
        testCase "parse package with name, description, version and string" $
                parseDatabase "package {name foo; version 1.2a; description test}" @?=
                Right db4,
        testCase "longer Version" $
                parseDatabase "package {name foo; version 1a.2a.45; description test}" @?=
                Right db5,
        -- Case doesn't matter for keywords
        testCase "Case insensitiveness" $
                parseDatabase "pAckAgE {nAmE foo; vErSiOn 1a.2a.45; deSCripTion test}" @?=
                Right db5,
        testCase "Case insensitiveness" $
                parseDatabase "pAckAgE {nAmE foo; vErSiOn 1a.2a.45; deSCripTion test; requires requires requires requires}" @?=
                Right db5
    ]
     where
       ver = V [VN 1 ""]
       pname = P "foo"
       pkg = Pkg pname ver  "" []
       db1 = DB [pkg,pkg,pkg]
       pkg2 = Pkg pname ver  "test" []
       db2 = DB [pkg2]
       ver2 = V [VN 1 "", VN 2 ""]
       pkg3 = Pkg pname ver2 "test" []
       db3 = DB [pkg3]
       ver3 = V [VN 1 "", VN 2 "a"]
       pkg4 = Pkg pname ver3 "test" []
       db4 = DB [pkg4]
       ver4 = V [VN 1 "a", VN 2 "a", VN 45 ""]
       pkg5 = Pkg pname ver4 "test" []
       db5 = DB [pkg5]


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

