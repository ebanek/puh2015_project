module EmailNotifications (
    Configuration,
    compileTemplate,
    readConfig,
    readConfigFile,
    sendMails,
    Template,
    Three (..)
) where

import Utility (readJSONConf, Three (..), Configuration (..))
import EmailParse (compileText, compileString)
import Network.Mail.SMTP (sendMailWithLogin', UserName, Password, sendMail')
import Network.Mail.Mime (Mail (..), simpleMail', Address(..))
import Network.Socket (HostName, PortNumber)
import Data.Text (Text, unpack, pack)
import Data.Map (Map (..))
import Control.Monad (sequence_)
import Control.Monad.Error
import Data.Text.Lazy (fromStrict)

type Template = Text

compileTemplate :: Template -> Map String Three -> Either String Text
compileTemplate template map = compileText template map

--this function should not handle the possibility of invalid file
readConfig :: IO (Maybe Configuration)
readConfig = readConfigFile defaultFile

readConfigFile :: FilePath -> IO (Maybe Configuration)
readConfigFile filePath = readJSONConf filePath 

data MailType = Type1 HostName PortNumber | 
                Type2 HostName PortNumber UserName Password

convertToMailType :: Configuration -> MailType
convertToMailType conf = case conf of
    Configuration {host = txt, port = pt, username = Nothing, password = Nothing} 
        -> Type1 (unpack txt) (toEnum pt) 
    Configuration {host = txt, port = pt, username = Just uname, password = Just pass}
        -> Type2 (unpack txt) (toEnum pt) (unpack uname) (unpack pass)
    otherwise -> error "Invalid configuration file!"

sendMails :: Configuration -> Text -> [String] -> IO ()
sendMails conf txt str = do
    sequence_ actions
    where actions = map (\s -> sendOneMail conf txt $ getMail conf txt s) str

sendOneMail :: Configuration -> Text -> Mail -> IO ()
sendOneMail conf txt mejl = do
    case convertedConf of
        Type1 hn pn -> (sendMail' hn pn mejl)
        Type2 hn pn un pass -> sendMailWithLogin' hn pn un pass mejl
    where convertedConf = convertToMailType conf   

defaultFile = "sample.json"

getMail :: Configuration -> Text -> String -> Mail
getMail conf txt email = simpleMail' 
        (Address Nothing (pack email)) 
        (Address Nothing (senderEmail conf))
        subj (fromStrict txt)
    where subj = case (subject conf) of
            Nothing -> pack ""
            Just tx -> tx

-- sendMail' :: HostName -> PortNumber -> Mail -> IO ()
-- sendMailWithLogin' :: HostName -> PortNumber -> UserName -> Password -> Mail -> IO ()

defEmails = ["erik.banek@gmail.com", "yolo@solo.com"]
mainTest = do
    conf <- readConfig
    case conf of
        Nothing -> putStrLn "Nothing is done, configuration path invalid." 
        Just c  -> sendMails c (pack "Ovo je tekst mog testnog maila.") defEmails
    
