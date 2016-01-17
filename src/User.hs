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


import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import Control.Monad.Logger (MonadLogger, monadLoggerLog)
import Control.Applicative  (pure)
import Data.Pool
import Role
import DBConfig
import Data.Word
import Control.Exception
import Data.Typeable


instance MonadLogger IO where
    monadLoggerLog _ _ _ = pure $ pure ()

-- | Custom Exception types
data NoSuchUserException = NoSuchUserException deriving (Show, Typeable)
instance Exception NoSuchUserException

data UserExistsException = UserExistsException deriving (Show, Typeable)
instance Exception UserExistsException

-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String

-- -- | The user's role in the course
-- data Role = Student Integer -- Academic Year shorthand (2015 for 2015/16)
--     | TA Integer Integer -- AY shorthand range (inclusive)
--     | Professor deriving (Eq, Ord, Show, Read)

-- | A user (the definition can be bigger)
-- data User = User {
--     identifier :: UserIdentifier,
--     email :: String,
--     pwdHash :: String,
--     role :: Role
--     } deriving (Eq, Show)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    identifier UserIdentifier maxlen=10
    email String
    pwdHash String
    role Role
    UniqueIdentifier identifier
    deriving Show Eq
|]

getConnInfo :: DBConfig -> ConnectInfo
getConnInfo x = defaultConnectInfo {
    connectHost     = hostname x,
    connectPort     = fromIntegral (port x) :: Word16,
    connectUser     = username x,
    connectPassword = password x,
    connectDatabase = database x
}


connCnt :: Int
connCnt = 10

databaseProvider :: SqlPersistM a -> IO a
databaseProvider action = do
    cfg <- parseConfigFile "../database.cfg"
    let connInfo = getConnInfo cfg
    withMySQLPool connInfo connCnt $ \pool ->
        flip runSqlPersistMPool pool $ do
            runMigration migrateAll
            action

-- | Takes a user identifier, e-mail, password and role.
-- | Performs password hashing and stores the user into the
-- | database, returning a filled User. If creating it fails (e.g.
-- | the user identifier is already taken), throws an appropriate
-- | exception.
createUser :: UserIdentifier -> String -> String -> Role -> IO User
-- createUser jmbag mail hash role = withMySQLPool connInfo connCnt $ \pool -> do
--     flip runSqlPersistMPool pool $ do
--         runMigration migrateAll
--         userId <- insert $ User jmbag mail hash role
--         liftIO $ return $ User jmbag mail hash role

createUser jmbag mail hash role = databaseProvider $ do
    let user = User jmbag mail hash role
    f <- insertBy user
    case f of
         Left (Entity uid _) -> throw UserExistsException
         Right uid           -> liftIO $ return user


-- | Updates a given user. Identifies it by the UserIdentifier (or
-- | maybe database id field, if you added it) in the User and overwrites
-- | the DB entry with the values in the User structure. Throws an
-- | appropriate error if it cannot do so; e.g. the user does not exist.
updateUser :: User -> IO ()
updateUser = undefined

-- | Deletes a user referenced by identifier. If no such user or the
-- | operation fails, an appropriate exception is thrown.
deleteUser :: UserIdentifier -> IO ()
deleteUser = undefined

-- | Lists all the users
listUsers :: IO [User]
listUsers = undefined

-- | Lists all users in a given role
listUsersInRole :: Role -> IO [User]
listUsersInRole = undefined

-- | Fetches a single user by identifier
getUser :: UserIdentifier -> IO User
getUser id =  databaseProvider $ do
    maybeUser <- getBy $ UniqueIdentifier id
    case maybeUser of
         Nothing                -> throw NoSuchUserException
         Just (Entity uid user) -> liftIO $ return user

-- | Checks whether the user has a role of AT LEAST X in a given academic
-- | year.
isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear = undefined
