{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module User where

import           Control.Applicative    (pure, (<$>))
import           Control.Exception
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (MonadLogger, monadLoggerLog)
import           Crypto.PasswordStore
import           Data.ByteString.Char8  (pack, unpack)
import           Data.IORef
import           Data.Pool
import           Data.Typeable
import           Data.Word
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import           DatabaseAccess
import           DBConfig
import           Role
import           System.IO.Unsafe


-- | Custom Exception types
data NoSuchUserException = NoSuchUserException deriving (Show, Typeable)
instance Exception NoSuchUserException

data UserExistsException = UserExistsException deriving (Show, Typeable)
instance Exception UserExistsException

-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String



share [mkPersist sqlSettings, mkMigrate "migrateUsers"] [persistLowerCase|
User
    identifier UserIdentifier maxlen=10
    email String
    pwdHash String
    role Role
    UniqueIdentifier identifier
    deriving Show Eq
|]

databaseProviderUsers :: SqlPersistM a -> IO a
databaseProviderUsers action = abstractDatabaseProvider migrateUsers action


hashStrength :: Int
hashStrength = 14


-- | Takes a user identifier, e-mail, password and role.
-- | Performs password hashing and stores the user into the
-- | database, returning a filled User. If creating it fails (e.g.
-- | the user identifier is already taken), throws an appropriate
-- | exception.
createUser :: UserIdentifier -> String -> String -> Role -> IO User
createUser jmbag mail pwd role = databaseProviderUsers $ do
    hash <- liftIO $ hashPassword pwd
    let user = User jmbag mail hash role

    f <- insertBy user
    case f of
         Left (Entity usr _) -> throw UserExistsException
         Right uid           -> liftIO $ return user


-- | Updates a given user. Identifies it by the UserIdentifier (or
-- | maybe database id field, if you added it) in the User and overwrites
-- | the DB entry with the values in the User structure. Throws an
-- | appropriate error if it cannot do so; e.g. the user does not exist.
updateUser :: User -> IO ()
updateUser user = databaseProviderUsers $ do
    let id = userIdentifier user
    maybeUser <- getBy $ UniqueIdentifier id
    case maybeUser of
         Nothing             -> throw NoSuchUserException
         Just (Entity uid _) -> replace uid user
    return ()


-- | Deletes a user referenced by identifier. If no such user or the
-- | operation fails, an appropriate exception is thrown.
deleteUser :: UserIdentifier -> IO ()
deleteUser id = databaseProviderUsers $ do
    exists <- liftIO $ existsUser id
    if exists then deleteBy $ UniqueIdentifier id else
        throw NoSuchUserException

    return ()

-- | Lists all the users
listUsers :: IO [User]
listUsers = databaseProviderUsers $ do
    users <- selectList [] []
    liftIO $ return $ map unwrapEntity users

-- | Lists all users in a given role
listUsersInRole :: Role -> IO [User]
listUsersInRole role = databaseProviderUsers $ do
    users <- selectList [UserRole ==. role] []
    liftIO $ return $ map unwrapEntity users

-- | Fetches a single user by identifier
getUser :: UserIdentifier -> IO User
getUser id =  databaseProviderUsers $ do
    maybeUser <- getBy $ UniqueIdentifier id
    liftIO $ return $ case maybeUser of
         Nothing              -> throw NoSuchUserException
         Just (Entity _ user) -> user


-- | Checks whether the user has a role of AT LEAST X in a given academic
-- | year.
isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear = undefined

-- | Utility function for unwrapping data from Entity context.
unwrapEntity :: Entity a -> a
unwrapEntity (Entity id x) = x

-- | Utility function for checking if user with given id
-- | exists in database.
existsUser :: UserIdentifier -> IO Bool
existsUser id = databaseProviderUsers $ do
    maybeUser <- getBy $ UniqueIdentifier id
    return $ case maybeUser of
         Nothing                -> False
         Just (Entity uid user) -> True

hashPassword :: String -> IO String
hashPassword pwd = unpack <$> makePassword (pack pwd) hashStrength

comparePwdWithHash :: String -> String -> IO Bool
comparePwdWithHash pwd trueHash = return $ verifyPassword (pack pwd) (pack trueHash)

