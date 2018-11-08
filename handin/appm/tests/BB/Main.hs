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
        parser,
        example--,
        --normalize
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
            Just [(P "Test",(True,V [VN 4 ""],V [VN 6 ""])),(P "Test2",(False,V [VN 3 "z"],V [VN 7 ""])),(P "Test3", (False, V [VN 1 ""], V [VN 10 ""]))],
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
                 parseDatabase "package {name foo;description \"test\"}" @?=
                 Right db2,
        testCase "parse package with name and description" $
                 parseDatabase "package {name foo;description \"test\"}" @?=
                 Right db2,
        testCase "parse package with name, description, version" $
                parseDatabase "package {name foo; version 1.2; description \"test\"}" @?=
                Right db3,
        testCase "parse package with name, description, version and string" $
                parseDatabase "package {name foo; version 1.2a; description \"test\"}" @?=
                Right db4,
        testCase "longer Version" $
                parseDatabase "package {name foo; version 1a.2a.45; description \"test\"}" @?=
                Right db5,
        -- pName hyphen, end hyphen also allowed
        testCase "Package name hypens" $
                parseDatabase "package {name 123-wewe-RR-}" @?=
                Right (DB [Pkg (P "123-wewe-RR-") (V [VN 1 ""])  "" []]),
        testCase "Package name strings" $
                parseDatabase "package {name \"123-wewe-RR-\"}" @?=
                Right (DB [Pkg (P "123-wewe-RR-") (V [VN 1 ""])  "" []]),
        -- Case doesn't matter for keywords
        testCase "Case insensitiveness" $
                parseDatabase "pAckAgE {nAmE foo; vErSiOn 1a.2a.45; deSCripTion \"test\"}" @?=
                Right db5,
        -- Dependencies Tests
        testCase "Deps conflicts and requires" $
                parseDatabase "package {name foo2; version 1a.2a.45; description \"test\"; requires foo < 2}" @?= --requires foo < 1.2 , foo >= 3;
                Right (DB [Pkg {name = P "foo2", ver = V [VN 1 "a",VN 2 "a",VN 45 ""],
                desc = "test", deps = [(P "foo",(True,V [VN 0 ""],V [VN 2 ""]))]}]),
        testCase "Deps requires range overwrite" $
                parseDatabase "package {name foo2; requires foo < 3 , foo >= 8.0.0}" @?=
                Right (DB [Pkg {name = P "foo2", ver = V [VN 1 ""], desc = "",
                deps = [(P "foo",(True,V [VN 3 ""],V [VN 8 "",VN 0 "",VN 0 ""]))]}]),
        testCase "Deps self referential" $
                parseDatabase "package {name foo; requires foo < 3 , foo >= 8.0.0}" @?=
                Right (DB [Pkg {name = P "foo", ver = V [VN 1 ""], desc = "", deps = []}]),
        testCase "Deps requires fixed range" $
                        parseDatabase "package {name foo2; requires foo < 3, foo >= 8.0.0a}" @?=
                        Right (DB [Pkg {name = P "foo2", ver = V [VN 1 ""], desc = "",
                        deps = [(P "foo",(True,V [VN 3 ""],V [VN 8 "",VN 0 "",VN 0 "a"]))]}]),
        testCase "Deps requires fixed range requires and conflicts" $
                         parseDatabase "package {name foo2; requires foo < 3 , foo >= 8.0.0a; conflicts bar < 3 , bar >= 8}" @?=
                         Right (DB [Pkg {name = P "foo2", ver = V [VN 1 ""], desc = "",
                         deps = [(P "foo",(True,V [VN 3 ""],V [VN 8 "",VN 0 "",VN 0 "a"])),
                         (P "bar",(False,V [VN 3 ""],V [VN 8 ""]))]}]),
        testCase "Deps different package names" $
                         parseDatabase "package {name foo2; requires foo < 3, bar >= 8.0.0a; conflicts bar < 3 , foo >= 8}" @?=
                         Right (DB [Pkg {name = P "foo2", ver = V [VN 1 ""], desc = "",
                         deps = [(P "foo",(True,V [VN 3 ""],V [VN 8 ""])),
                         (P "bar",(True,V [VN 3 ""],V [VN 8 "",VN 0 "",VN 0 "a"]))]}]),
        -- doesn't work to change the lower, greater equal
        testCase "Low/High changed" $
                         parseDatabase "package {name foo2; requires foo >=3 , bar < 8.0.0;}" @?=
                         Left "\"Parse Error\" (line 1, column 38):\nunexpected \",\"\nexpecting space or \"}\"",
         -- Whitespace and other more special things
        testCase "whitespaces pkg and name" $
        parseDatabase "package     {name        foo2;       requires       foo     <      3    ,      foo    >=      8.0.0a; conflicts bar < 3 , bar >= 8   }" @?=
            Right (DB [Pkg {name = P "foo2", ver = V [VN 1 ""], desc = "",
                                     deps = [(P "foo",(True,V [VN 3 ""],V [VN 8 "",VN 0 "",VN 0 "a"])),
                                     (P "bar",(False,V [VN 3 ""],V [VN 8 ""]))]}])
    ]
     where
       ver = V [VN 1 ""]
       pname = P "foo"
       pname2 = P "foo2"
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
       consts = [(P "foo",(True,V [VN 1 "", VN 2 ""],V [VN 3 ""])),(P "bar",(False,V [VN 0 ""],V [VN 2 ""]))]
       pkg6 = Pkg pname2 ver4 "test" consts
       db6 = DB [pkg6]

-- Parser Example
example = testGroup "Example DB" [
    testCase "Parse Example DB" $ parseDatabase "package { name foo; version 2.3; description \"The foo application\"; requires bar >= 1.0} package { name bar; version 1.0; description \"The bar library\"} package { name bar; version 2.1; description \"The bar library, new API\";  conflicts baz < 3.4, baz >= 5.0.3} package { name baz; version 6.1.2;}"
    @?=  Right (DB [Pkg {name = P "foo", ver = V [VN 2 "",VN 3 ""],
             desc = "The foo application",
             deps = [(P "bar",(True,V [VN 1 "",VN 0 ""],V [VN 1000000 ""]))]},
        Pkg {name = P "bar", ver = V [VN 1 "",VN 0 ""],
             desc = "The bar library", deps = []},
        Pkg {name = P "bar", ver = V [VN 2 "",VN 1 ""],
             desc = "The bar library, new API",
             deps = [(P "baz",(False,V [VN 3 "",VN 4 ""],V [VN 5 "",VN 0 "",VN 3 ""]))]},
        Pkg {name = P "baz", ver = V [VN 6 "",VN 1 "",VN 2 ""], desc = "", deps = []}])
    ]

-- normalize = testGroup "Normalize" [
--         testCase "small normalize" $ normalize
--     ]

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

