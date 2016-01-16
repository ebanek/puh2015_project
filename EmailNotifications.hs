module EmailNotifications (
    Configuration,
    compileTemplate,
    readConfig,
    readConfigFile,
    sendMail,
    Template,
    Three (..)
) where

import Utility (readJSONConf, Three (..), Configuration)
import EmailParse (compileText, compileString)
import Network.Mail.SMTP hiding (sendMail)
import Data.Text (Text)
import Data.Map (Map)
import Control.Monad.Error

type Template = Text

compileTemplate :: Template -> Map String Three -> Either String Text
compileTemplate template map = compileText template map

--this function should not handle the possibility of invalid file
readConfig :: IO (Maybe Configuration)
readConfig = readConfigFile defaultFile

readConfigFile :: FilePath -> IO (Maybe Configuration)
readConfigFile filePath = readJSONConf filePath 

sendMail :: Configuration -> Text -> [String] -> IO ()
sendMail = undefined

defaultFile = "sample.json"
