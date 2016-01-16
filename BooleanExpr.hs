module BooleanExpr (
    boolean
) where

import Data.Map
import Utility (Three(..), regularParse)
import Text.Parsec (ParseError)
import Text.Parsec.Char (letter, char, string)
import Text.ParserCombinators.Parsec (try, oneOf, Parser, many1, eof
                                     ,manyTill, lookAhead)
import Control.Applicative (many, (<|>))
import Control.Monad (void)

boolean :: Map String Three -> String -> Either String Bool
boolean map s = case either of 
    Left pError -> Left ("Parse error occured: " ++ (show pError))
    Right boolE -> Right $ evalExpr boolE
    where either = regularParse (simpleExpr map) s
          

evalExpr :: BoolExpr -> Bool
evalExpr (Var b)     = b
evalExpr (Or b1 b2)  = (evalExpr b1) || (evalExpr b2)
evalExpr (And b1 b2) = (evalExpr b1) && (evalExpr b2)
evalExpr (Not b)     = not (evalExpr b)

data BoolExpr = Var Bool 
              | Or BoolExpr BoolExpr
              | Not BoolExpr
              | And BoolExpr BoolExpr
                deriving (Eq, Show)

--helper functions

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
        x <- p
        whitespace
        return x

-- terms

boolFromMap :: Map String Three -> String -> Either String Bool
boolFromMap map s = case inMap of
    Just (Snd b)  -> Right b
    _             -> Left "Value was not mapped to a Bool!"
    where inMap = Data.Map.lookup s map

parensL :: Map String Three 
            -> (Map String Three -> Parser BoolExpr) 
            -> Parser BoolExpr
parensL map pars = do
    whitespace
    void $ lexeme $ char '('
    expr <- pars map
    void $ lexeme $ char ')'
    return expr

parseMapValue :: Map String Three -> Parser BoolExpr
parseMapValue map = do
    whitespace
    s <- lexeme $ many letter
    let bEither = boolFromMap map s
    case bEither of
        Left s  -> fail s
        Right b -> return $ Var b

term :: Map String Three 
            -> (Map String Three -> Parser BoolExpr)
            -> Parser BoolExpr
term map pars = try (parensL map pars) <|> (parseMapValue map)

myTerm :: Map String Three -> Parser BoolExpr
myTerm map = term map simpleExpr

-- now expressions
notBoolExpr :: Map String Three -> Parser BoolExpr
notBoolExpr map = do
    whitespace
    void $ lexeme $ string "not"
    e0 <- myTerm map
    return $ (Not e0)
    

parseBoolExpr :: Map String Three -> Parser BoolExpr
parseBoolExpr map = do
     whitespace
     e0 <- myTerm map
     oper <- lexeme $ many1 letter
     e1 <- myTerm map
     let eitherExpr = getBoolExpr oper e0 e1
     case eitherExpr of
        Left s      -> fail s
        Right bExpr -> return bExpr

getBoolExpr :: String -> BoolExpr -> BoolExpr -> Either String BoolExpr
getBoolExpr op b1 b2 = case op of
    "or"  -> Right (Or b1 b2)
    "and" -> Right (And b1 b2)
    _     -> Left "Unrecognized boolean operator."

parseExpr :: Map String Three -> Parser BoolExpr
parseExpr map = (notBoolExpr map) <|> (parseBoolExpr map)

simpleExpr :: Map String Three -> Parser BoolExpr
simpleExpr map = try (parseExpr map) <|> (myTerm map)


testMap = fromList[
                ("someBool", Snd True), 
                ("aList", Thd ["yolo", "dude"]),
                ("mapped", Fst "You have accessed a mapped value."),
                ("otherMapped", Fst "I am a snail."),
                ("fTrue", Snd True), 
                ("sTrue", Snd True), 
                ("fFalse", Snd False)] 

testString = "Well, fdsjibalfdiblsadblf @if{someBool and (not fFalse)} this @if{someBool} nested madafaka @endif{} is my @m{mapped} first parser.@endif{} @m{mapped}. Makes you wonder @m{otherMapped}. Makes you wonder. @end{}"

