{-# LANGUAGE OverloadedStrings #-}

module Utility (
    Three (..),
    extract,
    regularParse,
    Configuration,
    readJSONConf
) where

import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec (ParseError, parse)

import Data.Aeson
import Data.Text
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as ByteStr

extract :: Three -> String
extract (Fst s)  = s
extract (Snd b)  = show b
extract (Thd xs) = show xs

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

data Three = Fst String 
            | Snd Bool 
            | Thd [String] deriving (Show, Eq)

data Configuration = Configuration {
    host :: Int,
    port :: Int,
    senderEmail :: Text,
    username :: Text,
    password :: Text 
} deriving (Show, Eq)


instance FromJSON Configuration where
    parseJSON (Object v) =
        Configuration <$> v .: (pack "host")
                      <*> v .: (pack "port")
                      <*> v .: (pack "senderEmail")
                      <*> v .: (pack "username")
                      <*> v .: (pack "password")
    parseJSON _ = mzero

sampleConf :: IO (Maybe Configuration)
sampleConf = do
    jsonFile <- ByteStr.readFile "sample.conf"
    let conf = decode jsonFile :: Maybe Configuration
    return conf

readJSONConf :: FilePath -> IO (Maybe Configuration)
readJSONConf file = do
    jsonFile <- ByteStr.readFile file
    let conf = decode jsonFile :: Maybe Configuration
    return conf
