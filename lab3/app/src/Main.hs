module Main where

import Dot
import Automata

type Alphabet = String

data Options = Optoins {
    tolerance :: !Int,
    equivalence :: !Int,
    pumping :: !Int,
    alphabet :: Alphabet
}

main :: IO ()
main = return () :: IO ()
