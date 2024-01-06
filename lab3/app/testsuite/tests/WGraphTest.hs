module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import WGraph

graph =
    WGraph [
        (0, [(1, 0)]),
        (1, [(2, 0)]),
        (2, [(3, 0), (4, 0)]),
        (3, [(1, 0)]),
        (4, [(5, 0)]),
        (5, [(6, 0)]),
        (6, [(4, 0)])
    ]

-- 0 -> 1 -> 2 -> 4 -> 5
--      ^   /     ^   /
--      |  /      |  /
--      3 <       6 <

tests =
    [
        testGroup "getting graph components"
        [
            testCase
            "vertices"
            undefined,
            testCase
            "edges"
            undefined,
            testCase
            "transitions"
            undefined
        ],
        testGroup "adding graph components"
        [
            testCase
            "vertices"
            undefined,
            testCase
            "transitions"
            undefined
        ],
        testGroup "getting graph characteristics"
        [
            testCase
            "input degrees"
            undefined,
            testCase
            "reachability matrix"
            undefined
        ],
        testGroup "getting graph complex components"
        [
            testCase
            "subgraph"
            undefined,
            testCase
            "strong component"
            undefined
        ]
    ]

main = defaultMain tests
