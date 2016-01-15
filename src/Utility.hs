module Utility (
    Three (..),
    extract,
    regularParse
) where

import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec (ParseError, parse)


extract :: Three -> String
extract (Fst s)  = s
extract (Snd b)  = show b
extract (Thd xs) = show xs

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

data Three = Fst String 
            | Snd Bool 
            | Thd [String] deriving (Show, Eq)
