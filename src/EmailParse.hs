import Text.Parsec (ParseError, parse)
import Text.Parsec.Char (char, letter, satisfy, anyChar)
import Text.ParserCombinators.Parsec (Parser, try, noneOf, many1
                                      ,manyTill)
import Control.Applicative (many, (<*>), (<|>))
import Control.Monad (void)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

--([Char], [Char])
readToken :: Parser ([Char], [Char])
readToken = do
     void $ char '@'
     func <- many1 letter
     void $ char '{'
     args <- many (noneOf "}")
     void $ char '}'
     return (func, args)

readAllTokens :: Parser [([Char], [Char])]
readAllTokens = many one
    where one = readToken <|> (anyChar >> one) 

readText :: Parser [Char]
readText = manyTill anyChar $ try readToken
