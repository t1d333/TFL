{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}

module WGraph (
    WGraph (..),
    getTransitions,
    addTransitions,
    getVertices,
    addVertices
) where

import qualified Data.Function as Function
import qualified Data.List as List

import ListUtils

newtype WGraph v w = WGraph [(v, [(v, w)])]

getEdges :: Ord v => WGraph v w -> [(v, v)]
getEdges (WGraph g) = foldr (insertManyUniqueBy compare . (\(f, s) -> List.map (\x -> (f, fst x)) s)) [] g

getTransitions :: Eq v => v -> WGraph v w -> [(v, w)]
getTransitions vertex (WGraph d) =
    case List.lookup vertex d of
        Just ts -> ts
        Nothing -> error "WGraph.getTransitions : no such vertex in graph"

getNeighbours :: Eq v => v -> WGraph v w -> [v]
getNeighbours v (WGraph g) =
    case List.lookup v g of
        Just ts -> nubSorted $ List.map fst ts
        Nothing -> error "WGraph.getNeighbours : no such vertex in graph"
    where 
        nubSorted arr = undefined
            where
                nubSorted' result [] = List.reverse result
                nubSorted' [] arr = nubSorted' [head arr] (tail arr)
                nubSorted' result arr | head result == head arr = nubSorted' result (tail arr)
                                      | otherwise = nubSorted' (head arr : result) (tail arr)

addTransitions :: (Eq v, Ord v, Ord w) => v -> [(v, w)] -> WGraph v w -> WGraph v w
addTransitions vfrom vsto (WGraph g) =
    WGraph $ replaceOneBy ((vfrom ==) . fst) (vfrom, ts) $
    insertManyUniqueBy (Function.on compare fst) (List.map ((, []) . fst) vsto) g
    where
        ts = insertManyUniqueBy compare vsto $ getTransitions vfrom (WGraph g)

getVertices :: WGraph v w -> [v]
getVertices (WGraph g) = List.map fst g

addVertices :: Ord v => [v] -> WGraph v w -> WGraph v w
addVertices vs (WGraph g) = WGraph $ insertManyUniqueBy (Function.on compare fst) (List.map (, []) vs) g

getReachabilityMatrix :: WGraph v w -> [(v, [v])]
getReachabilityMatrix (WGraph g) = undefined
    where
        getReachabilityMatrix' marker matrix = undefined

getInputDegrees :: Ord v => WGraph v w -> [(v, Int)]
getInputDegrees g = getInputDegrees' (List.map (, 0) $ getVertices g) $ getEdges g
    where
        getInputDegrees' :: Eq v => [(v, Int)] -> [(v, v)] -> [(v, Int)]
        getInputDegrees' result [] = result
        getInputDegrees' result edges =
            case List.lookup (snd $ head edges) result of
                Just count -> getInputDegrees' (replaceOneBy ((snd $ head edges, count) == ) (snd $ head edges, count + 1) result) (tail edges)
                Nothing -> error "WGraph.getInputDegrees : vertex not found"

data Color = White | Gray | Black
    deriving (Eq, Ord)

isGraphHasCircuits :: Eq v => WGraph v w -> Bool
isGraphHasCircuits g = isGraphHasCircuits' $ List.map (, White) vertices
    where
        vertices = getVertices g
        anyKey = List.head vertices
        toVisit = getNeighbours anyKey g
        paint s c = replaceOneBy (Function.on (==) fst (s, c)) (s, c)

        isGraphHasCircuits' marker = 
            case List.find ((== White) . snd) marker of
                Just (v, _) ->
                    case dfs marker v of
                        Just marker' -> isGraphHasCircuits' marker
                        Nothing -> False
                Nothing -> False

        dfs visited current
            | null neighbours = Just $ paint current Black visited
            | otherwise =
                  case List.lookup (head neighbours) visited of
                      Just White ->
                          case dfs (paint (head neighbours) Gray visited) (head neighbours) of
                              Just x -> dfs x current
                              Nothing -> Nothing
                      Just Gray -> Nothing
                      Just Black -> dfs visited current
                      Nothing -> error "WGraph.isGraphHasCircuits : dfs error"
            where
                neighbours = getNeighbours current g


getSubgraph :: Eq v => [v] -> WGraph v w -> WGraph v w
getSubgraph vs (WGraph g) = WGraph $ List.map (\(v, ts) -> (v, vertexFilter ts)) $ vertexFilter g
    where
        vertexFilter = List.filter ((`List.elem` vs) . fst)

getStrongComponent :: WGraph v w -> WGraph v w
getStrongComponent = undefined