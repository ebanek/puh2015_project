module EmailParse (
    compileText,
    compileString
) where 

import Utility
import BooleanExpr (boolean)
import Text.Parsec (ParseError, parse)
import Text.Parsec.Char (char, letter, satisfy, anyChar, string)
import Text.ParserCombinators.Parsec (Parser, try, noneOf, many1
                                      ,manyTill, lookAhead)
import Control.Applicative (many, (<*>), (<|>), pure, (<$>), (*>))
import Control.Monad (void, fail)
import Data.Map
import Data.Text

compileText :: Text -> Map String Three -> Either String Text
compileText txt map = case result of
    Left  s -> Left s
    Right s -> Right (pack s)
    where str = unpack txt
          result = compileString str map

compileString :: String -> Map String Three -> Either String String
compileString str map = case result of
    Left pError -> Left ("Parsing error occured " ++ (show pError))
    Right s     -> Right s
    where result = regularParse (parseWithMap map) str

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
    s <- parseWithMap map
    case b of
        Left pError -> fail ("Evaluating boolean failed: " ++ pError)
        Right True  -> (s ++) <$> parseWithMap map
        Right False -> (parseWithMap map)
    where b = boolean map expr

fromMapAndParse :: String -> Map String Three -> Parser String
fromMapAndParse args map = do
    let fromMap = Data.Map.lookup args map
    case fromMap of
        Nothing -> fail ("Key was not mapped to any value " ++ args)
        Just t  -> do s <- pure $ extract $ t
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


testMap = fromList[
                ("someBool", Snd True), 
                ("aList", Thd ["yolo", "dude"]),
                ("mapped", Fst "You have accessed a mapped value."),
                ("otherMapped", Fst "I am a snail."),
                ("fTrue", Snd True), 
                ("sTrue", Snd True), 
                ("fFalse", Snd False)] 

testString = "Well, fdsjibalfdiblsadblf @if{(not someBool) or (not fFalse)} this @if{someBool} nested madafaka @endif{} is my @m{mapped} first parser.@endif{} @m{mapped}. Makes you wonder @m{otherMapped}. Makes you wonder. @end{}"

