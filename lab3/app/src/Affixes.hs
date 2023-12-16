module Affixes where

import Automata

type Alphabet = String

check :: Alphabet -> Maybe Automata
check = undefined

affixes :: Alphabet -> [Automata]
affixes alphabet = affixes' [] alphabet
    where
        affixes' alphabet [] =
            case check alphabet of
                Nothing -> []
                (Just automata) -> [automata]
        affixes' alphabet remains =
            affixes' (alphabet ++ [head remains]) (tail remains) ++
            affixes' alphabet (tail remains)
