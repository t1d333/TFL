{-# LANGUAGE TupleSections #-}
module Automata where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Dot

type State = Int
type Transition = (State, State, String)
type Transitions = Map.Map (State, String) State
data Automata = Automata
    { states :: Set.Set State
    , finalStates :: Set.Set State
    , transitions :: Transitions
    }

getTransitions :: Automata -> State -> [State]
getTransitions automata state = Map.elems $ Map.filterWithKey (\(s, _) _ -> s == state) $ transitions automata

data Color = White | Gray | Black

isAutomataHasCircuits :: Automata -> Bool
isAutomataHasCircuits a =
    case dfs marker anyKey toVisit of
        Just _ -> False
        Nothing -> True
    where
        anyKey = head $ Set.elems $ states a
        toVisit = getTransitions a anyKey
        marker = Map.insert anyKey Gray $ Map.fromAscList $ map (,White) (Set.elems $ states a)
        dfs :: Map.Map State Color -> State -> [State] -> Maybe (Map.Map State Color)
        dfs visited current [] = Just $ Map.insert current Black visited
        dfs visited current (s:ss) =
            case Map.lookup s visited of
                Just White -> case dfs (Map.insert s Gray visited) s (getTransitions a s) of
                    Just x -> dfs x current ss
                    Nothing -> Nothing
                Just Gray -> Nothing
                Just Black -> dfs visited current ss
                Nothing -> error "dfs error"


dotToAutomata :: DotGraph -> Automata
dotToAutomata dg = undefined
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
                        (Set.insert nc $ states a)
                        (finalStates a)
                        (transitions a)
                    where
                        nc = read n :: Int
                ([n], Just (DotAttribute "" "")) ->
                    Automata
                        (Set.insert nc $ states a)
                        (Set.insert nc $ finalStates a)
                        (transitions a)
                    where
                        nc = read n :: Int
                ([n1, n2], Just (DotAttribute "label" t)) ->
                    Automata
                        (Set.insert n1c $ states a)
                        (Set.insert n1c $ finalStates a)
                        (Map.insert (n1c, t) n2c $ transitions a)
                    where
                        n1c = read n1 :: Int
                        n2c = read n2 :: Int
                _ -> error "wrong amount of nodes or invalid attributes"