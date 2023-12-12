{-# LANGUAGE TupleSections #-}
module Automata where

import qualified Data.Map as Map
import qualified Data.Set as Set

type State = Int
type Transition = (State, State, Char)

type Automata = Map.Map (State, Char) State

getTransitions :: Automata -> State -> Map.Map (State, Char) State
getTransitions automata state = Map.filterWithKey (\(s, _) _ -> s == state) automata

getStates :: Automata -> [State]
getStates a = Set.elems $ Set.fromList $ map fst (Map.keys a) ++ Map.elems a

data Color = White | Gray | Black

isAutomataHasCircuits :: Automata -> Bool
isAutomataHasCircuits a =
    case dfs marker anyKey toVisit of
        Just _ -> False
        Nothing -> True
    where
        anyKey = head $ getStates a
        toVisit = Map.elems $ getTransitions a anyKey
        marker = Map.insert anyKey Gray $ Map.fromAscList $ map (,White) (getStates a)
        dfs :: Map.Map State Color -> State -> [State] -> Maybe (Map.Map State Color)
        dfs visited current [] = Just $ Map.insert current Black visited
        dfs visited current (s:ss) =
            case Map.lookup s visited of
                Just White -> case dfs (Map.insert s Gray visited) s (Map.elems $ getTransitions a s) of
                    Just x -> dfs x current ss
                    Nothing -> Nothing
                Just Gray -> Nothing
                Just Black -> dfs visited current ss
                Nothing -> error "dfs error"
