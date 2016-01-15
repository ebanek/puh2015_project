import EmailParse
import Data.Map
import Text.Parsec.Char (letter, char, string)
import Text.ParserCombinators.Parsec (try, oneOf, Parser, many1, eof
                                     ,manyTill, lookAhead)
import Control.Applicative (many, (<|>))
import Control.Monad (void)

boolean :: Map String Three -> String -> Bool
boolean map s = evalExpr parsedExpr
    where either = regularParse (parseBoolExpr map) s
          parsedExpr = case either of
              Left err   -> error (show err)
              Right expr -> expr 


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

boolFromMap :: Map String Three -> String -> Bool
boolFromMap map s = case inMap of
    Snd b -> b
    _     -> error "Value was not mapped to a Bool!"
    where inMap = map ! s

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
    return $ (Var $ boolFromMap map s)

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
     return $ (getBoolExpr oper e0 e1)

getBoolExpr :: String -> BoolExpr -> BoolExpr -> BoolExpr
getBoolExpr op b1 b2 = case op of
    "or"  -> (Or b1 b2)
    "and" -> (And b1 b2)
    _     -> error "Unrecognized boolean operator."

parseExpr :: Map String Three -> Parser BoolExpr
parseExpr map = (notBoolExpr map) <|> (parseBoolExpr map)

simpleExpr :: Map String Three -> Parser BoolExpr
simpleExpr map = try (parseExpr map) <|> (myTerm map)


testMap = fromList [("fTrue", Snd True), ("sTrue", Snd True), ("fFalse", Snd False)]
