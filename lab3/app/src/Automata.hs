module Automata where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Function as Function

import Dot

import WGraph (
    WGraph (..),
    getTransitions,
    addTransitions,
    getVertices,
    addVertices )

import ListUtils (
    replaceOneBy,
    insertOneUniqueBy )

type State = Int
type Transition = (State, State, String)
data Automata = Automata
    { adj :: WGraph State String
    , finalStates :: [State] }

dotToAutomata :: DotGraph -> Automata
dotToAutomata dg = dotGraphToAutomata dg $ Automata { adj = WGraph [], finalStates = [] }
    where
        dotGraphToAutomata :: DotGraph -> Automata -> Automata
        dotGraphToAutomata [] a = a
        dotGraphToAutomata (ds:dss) a =
            dotGraphToAutomata dss $
            dotStatementToAutomata ds a

        dotStatementToAutomata :: DotStatement -> Automata -> Automata
        dotStatementToAutomata (DotStatement ns attr) a =
            case (ns, attr) of
                ([n], Nothing) ->
                    Automata
                        { adj = addVertices [nc] $ adj a
                        , finalStates = finalStates a }
                    where
                        nc = read n :: Int
                ([n], Just (DotAttribute "" "")) ->
                    Automata
                        { adj = addVertices [nc] $ adj a
                        , finalStates = insertOneUniqueBy compare nc $ finalStates a}
                    where
                        nc = read n :: Int
                ([n1, n2], Just (DotAttribute "label" t)) ->
                    Automata
                        { adj = addTransitions n1c [(n2c, t)] $ adj a
                        , finalStates = finalStates a }
                    where
                        n1c = read n1 :: Int
                        n2c = read n2 :: Int
                _ -> error "wrong amount of nodes or invalid attributes"