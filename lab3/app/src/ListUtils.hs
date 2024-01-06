module ListUtils where

import qualified Data.List as List

replaceManyBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
replaceManyBy = replaceManyBy' []
    where
        replaceManyBy' result p from [] = List.reverse result
        replaceManyBy' result p [] to = List.reverse $ List.reverse to ++ result
        replaceManyBy' result p from to =
            case p (head from) (head to) of 
                LT -> replaceManyBy' result p (tail from) to
                EQ -> replaceManyBy' result p (tail from) (head from : tail to)
                GT -> replaceManyBy' (head to : result) p from (tail to)

replaceOneBy :: (a -> Bool) -> a -> [a] -> [a]
replaceOneBy p e = replaceManyBy (const predicate) [e]
    where
        predicate x
            | p x = EQ
            | otherwise = GT

insertManyUniqueBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertManyUniqueBy = insertManyUniqueBy' []
    where
        insertManyUniqueBy' result p [] to = List.reverse $ List.reverse to ++ result
        insertManyUniqueBy' result p from [] = List.reverse $ List.reverse from ++ result
        insertManyUniqueBy' result p from to =
            case p (head from) (head to) of
                LT -> insertManyUniqueBy' result p (tail from) (head from : to)
                EQ -> insertManyUniqueBy' result p (tail from) to
                GT -> insertManyUniqueBy' (head to : result) p from (tail to)

insertOneUniqueBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertOneUniqueBy p e = insertManyUniqueBy p [e]
