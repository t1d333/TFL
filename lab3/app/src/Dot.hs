{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Dot where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data DotAttribute = DotAttribute String String deriving Show
newtype DotEntity = DotEntity [String] deriving Show
data DotStatement = DotStatement DotEntity (Maybe DotAttribute) deriving Show
newtype DotGraph = DotGraph [DotStatement] deriving Show

parseGraph =
    do
        many $ noneOf "{"
        char '{'
        sl <- parseStatementList
        char '}'
        return $ DotGraph sl


parseStatementList = endBy parseStatement (char ';')

parseStatement :: GenParser Char st DotStatement
parseStatement =
    do 
        spaces
        e <- parseEntity
        a <- parseAttribute
        return $ DotStatement e a

parseEntity :: GenParser Char st DotEntity
parseEntity =
    do
        ns <- sepBy parseNode (string "->" >> spaces)
        return $ DotEntity ns

parseNode =
    do
        n <- many $ noneOf "-;[} "
        spaces
        return n

parseAttribute = 
    do
        spaces
        char '['
        an <- many $ noneOf "="
        char '='
        av <- many $ noneOf "]"
        char ']'
        return $ Just $ DotAttribute an av
    <|> return Nothing
