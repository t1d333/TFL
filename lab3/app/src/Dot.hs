{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Dot where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data DotAttribute = DotAttribute String String deriving Show
type DotEntity = [String]
data DotStatement = DotStatement DotEntity (Maybe DotAttribute) deriving Show
type DotGraph = [DotStatement]

parseGraph =
    do
        many $ noneOf "{"
        char '{'
        sl <- parseStatementList
        char '}'
        return sl


parseStatementList = endBy parseStatement (char ';')

parseStatement =
    do 
        spaces
        e <- parseEntity
        a <- parseAttribute
        return $ DotStatement e a

parseEntity :: GenParser Char st DotEntity
parseEntity = sepBy parseNode (string "->" >> spaces)

parseNode =
    do
        n <- many $ noneOf "-;[} "
        spaces
        return n

parseAttribute = 
    do
        spaces >> char '[' >> spaces
        an <- many $ noneOf "= "
        spaces >> char '=' >> spaces
        av <- many $ noneOf "] "
        spaces >> char ']'
        return $ Just $ DotAttribute an av
    <|> return Nothing
