{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Dot where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

data DotAttribute = DotAttribute String String deriving (Eq, Show)
type DotEntity = [String]
data DotStatement = DotStatement DotEntity (Maybe DotAttribute) deriving (Eq, Show)
type DotGraph = [DotStatement]

whitespaces = skipMany $ oneOf " \n"

parseDot = parse parseGraph "(unknown)"
parseGraph =
    do
        many $ noneOf "{"
        char '{'
        whitespaces
        sl <- parseStatementList
        char '}'
        return sl

parseStatementList = endBy parseStatement (char ';' >> whitespaces)

parseStatement =
    do 
        e <- parseEntity
        whitespaces
        a <- parseAttribute
        whitespaces
        return $ DotStatement e a

parseEntity :: GenParser Char st DotEntity
parseEntity = sepBy parseNode (string "->" >> whitespaces)

parseNode =
    do
        n <- many $ noneOf "-;[} "
        whitespaces
        return n

parseAttribute = 
    do
        char '[' >> whitespaces
        an <- many $ noneOf "= "
        whitespaces >> char '=' >> whitespaces
        av <- many $ noneOf "] "
        whitespaces >> char ']'
        return $ Just $ DotAttribute an av
    <|> return Nothing

-- digraph g { \n a; \n b; \n c; \n a -> b -> c [label="asdf"]; }