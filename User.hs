{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, EmptyDataDecls, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, DoAndIfThenElse #-}
module User (
    doMain,
    User,
    createUser,
    getUser,
    isRoleInYear,
    listUsers,
    deleteUser,
    listUsersInRole,
    updateUser
) where


import Data.Text (Text, unpack, pack)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)
-- imports for dumpTable
import Database.Persist.Sql (rawQuery, insert)
import Data.Conduit (($$))
import Data.Conduit.List as CL (mapM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, mfilter)

data Role = Student Integer 
          | TA Integer Integer
          | Professor deriving (Eq, Ord, Show, Read)

type UserIdentifier = String

data User = User {
    identifier :: UserIdentifier,
    email      :: String,
    pwdHash    :: String,
    role       :: Role
} deriving (Eq, Show)

--could not find how to make a field unique
share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Dbuser
    identifier Text
    email Text
    pwdHash Text
    role Text
    UniqueId identifier
    deriving Show
|]

dbLocation = "data.db"

doMain :: IO ()
doMain = runSqlite dbLocation $ do
    buildDatabase
    basic <- selectFirst [DbuserIdentifier ==. "00434"] []
    liftIO $ print basic


dumpTable :: IO ()
dumpTable = runSqlite dbLocation $ rawQuery "select * from Dbuser" [] $$ CL.mapM_ (liftIO . print)

buildDatabase = do
    runMigrationSilent migrateTables
    insert $ Dbuser "432" "some@fer.hr" "78tsd78f676vfgf5" "Student 2015"
    insert $ Dbuser "654654654" "other@pmf.hr" "54jhk3b65jb" "Professor"
    insert $ Dbuser "fdgsgffgddfg" "dhfj@tvz.hr" "somehas" "TA 2010 3010"
    insert $ Dbuser "43554345" "bla@pyth.hr" "34344354" "Student 2016" 
    insert $ Dbuser "cvcxvcxvc4" "other@vlada.hr" "54jhk3b6fsd5jb" "Professor"
    insert $ Dbuser "g332ddfg" "dhfj@kolinda.hr" "somehafdss" "TA 2010 1010"
    insert $ Dbuser "00434" "bla@ores.hr" "343fdsdffd44354" "Student 1016"


userToDB :: User -> Dbuser
userToDB user = Dbuser {
    dbuserIdentifier = (pack $ identifier user),
    dbuserEmail = (pack $ email user),
    dbuserPwdHash = (pack $ pwdHash user),
    dbuserRole = (pack $ show $ role user)
}
    

dbToUser :: Dbuser -> User
dbToUser dbus = User {
    identifier = (unpack $ dbuserIdentifier dbus), 
    email = (unpack $ dbuserEmail dbus), 
    pwdHash = (unpack $ dbuserPwdHash dbus), 
    role = read (unpack $ dbuserRole dbus)
}

getUser :: UserIdentifier -> IO (Either String User)
getUser uId = runSqlite dbLocation $ do
        basic <- selectFirst [DbuserIdentifier ==. (pack uId)] []
        case basic of
            Nothing                 -> return $ Left "No such user in base."
            Just (Entity id dbuser) -> return $ Right (dbToUser dbuser) 

createUser :: UserIdentifier -> String -> String -> Role -> IO (Either String User)
createUser uId mail pwd usRole = runSqlite dbLocation $ do
    basic <- selectFirst [DbuserIdentifier ==. (pack uId)] []
    case basic of
        Nothing             -> do void $ insert $ userToDB user
                                  return $ (Right user)        
        Just (Entity id db) -> return $ Left "User already exists in base."
    where user = (User uId mail pwd usRole)
    

isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear (User {role = r}) (Student _) x2 = case r of
    Student x1 -> x1 == x2
    TA y1 y2   -> (x2 >= y1 && x2 <= y2)
    Professor  -> True
isRoleInYear (User {role = r}) (TA _ _) x2 = case r of 
    Student _  -> False
    TA y1 y2   -> (x2 >= y1 && x2 <= y2)
    Professor  -> True
isRoleInYear (User {role = r}) (Professor) x2 = case r of
    Professor  -> True
    _          -> False

 
updateUser :: User -> IO (Either String ())
updateUser us = runSqlite dbLocation $ do
        basic <- selectFirst [DbuserIdentifier ==. (pack $ identifier us)] []
        case basic of
            Nothing            -> return $ Left "User does not exist in base."
            Just (Entity id _) -> do update id [DbuserEmail =. (pack $ email us),
                                                DbuserPwdHash =. (pack $ pwdHash us),
                                                DbuserRole =. (pack $ show $ role us)]
                                     return $ Right ()

hashIt :: String -> String
hashIt _ = "hashish"

deleteUser :: UserIdentifier -> IO ()
deleteUser uId = runSqlite dbLocation $ do
   deleteBy $ UniqueId $ pack uId 


isInRole :: Role -> User -> Bool
isInRole qRole (User {role = r}) = case (qRole, r) of
    (Student _, Student _) -> True
    (Professor, Professor) -> True
    (TA _ _, TA _ _)       -> True
    _                      -> False

--this should not be done this way, what if 
--size of database is too big
--also the repeating connection establishment..
--but too much work, this is a prototype
listUsersInRole :: Role -> IO [User]
listUsersInRole r = do
    users <- listUsers
    let filtered = filter (\u -> isInRole r u) users
    return filtered

listUsers :: IO [User]
listUsers = runSqlite dbLocation $ do
    basic <- selectList [] []
    return $ map (\(Entity _ dbus) -> dbToUser dbus) basic
