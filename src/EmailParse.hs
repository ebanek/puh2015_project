module EmailParse (
    Three,
    extract
) where 

import Text.Parsec (ParseError, parse)
import Text.Parsec.Char (char, letter, satisfy, anyChar, string)
import Text.ParserCombinators.Parsec (Parser, try, noneOf, many1
                                      ,manyTill, lookAhead)
import Control.Applicative (many, (<*>), (<|>), pure, (<$>), (*>))
import Control.Monad (void)
import Data.Map

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseWithMap :: Map String Three -> Parser String
parseWithMap map = do
    s <- readText
    (func, args) <- readToken
    case func of
        "if"     -> (s ++) <$> (ifAndParse args map)
        "m"      -> (s ++) <$> (fromMapAndParse args map)
        "endfor" -> return s
        "end"    -> return s 
        "endif"  -> return s

ifAndParse :: String -> Map String Three -> Parser String
ifAndParse expr map = do
    let b = isBooleanExpr expr map
    s <- parseWithMap map
    if b then 
        (s ++) <$> parseWithMap map
    else 
        (parseWithMap map)


fromMapAndParse :: String -> Map String Three -> Parser String
fromMapAndParse args map = do
    s <- pure $ extract $ map ! args
    next <- parseWithMap map
    return $ (s ++ next)

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

-- parses until token, but does not consume token! both lookAhead
-- and try are needed
readText :: Parser [Char]
readText = do
    s <- manyTill anyChar $ (try $ lookAhead readToken)
    return s

isBooleanExpr :: String -> Map String Three -> Bool
isBooleanExpr args map = True

data Three = Fst String | Snd Bool | Thd [String] deriving Show
testMap = fromList[
                ("someBool", Snd True), 
                ("aList", Thd ["yolo", "dude"]),
                ("mapped", Fst "You have accessed a mapped value."),
                ("otherMapped", Fst "I am a snail.")] 

testString = "Well, @if{someBool} this @if{someBool} nested madafaka @endif{} is my @m{maped} first parser.@endif{} @m{mapped}. Makes you wonder @m{otherMapped}. Makes you wonder. @end{}"

extract :: Three -> String
extract (Fst s)  = s
extract (Snd b)  = show b
extract (Thd xs) = show xs
