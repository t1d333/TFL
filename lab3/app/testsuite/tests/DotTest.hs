module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
    ( (@?),
      runTestTT,
      AssertionPredicable(assertionPredicate),
      Test(TestLabel, TestList) )

import Dot ( parseDot, DotStatement (DotStatement), DotAttribute (DotAttribute) )
import Paths_app

parseDotFromFile filename =
    do
        filepath <- getDataFileName filename
        input <- readFile filepath
        return $ parseDot input

test_parsing filename (Left e) = 
    predicate @? "File should be parsed with an error:\n" ++ e
    where 
        predicate =
            assertionPredicate $
            do
                parsed <- parseDotFromFile filename
                return $ case parsed of
                    (Left e') -> e == show parsed
                    (Right _) -> False

test_parsing filename (Right dg) =
    predicate @? "File should be successfully parsed"
    where
        predicate =
            assertionPredicate $
            do
                parsed <- parseDotFromFile filename
                return $ case parsed of
                    (Left _) -> False
                    (Right dg') -> dg == dg'

tests =
    [
        testGroup "dot parsing"
        [
            testCase
            "incorrect dot" $
            test_parsing "testsuite/data/incorrect-graph.dot" $
            Left "Left \"(unknown)\" (line 2, column 7):\nunexpected \"a\"\nexpecting \"->\", \"[\" or \";\"",
            testCase
            "correct dot" $
            test_parsing "testsuite/data/graph.dot" $
            Right
                [
                    DotStatement ["a"] Nothing,
                    DotStatement ["b"] Nothing,
                    DotStatement ["c"] Nothing,
                    DotStatement ["a", "b", "c"] $ Just $ DotAttribute "label" "\"asdf\""
                ]
        ]
    ]

main = defaultMain tests