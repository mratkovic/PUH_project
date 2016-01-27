{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module DatabaseAccess where

import           Control.Monad.Logger   (MonadLogger, monadLoggerLog)
import           Data.Word
import           Database.Persist.MySQL
import           DBConfig

instance MonadLogger IO where
    monadLoggerLog _ _ _ = pure $ pure ()

-- | Connection count.
connCnt :: Int
connCnt = 10

-- | Function converts DBConfig to ConnectInfo.
getConnInfo :: DBConfig -> ConnectInfo
getConnInfo x = defaultConnectInfo {
    connectHost     = hostname x,
    connectPort     = fromIntegral (port x) :: Word16,
    connectUser     = username x,
    connectPassword = password x,
    connectDatabase = database x
}
-- | Function loads default database connection credentials from file.
dbConnectInfo :: IO ConnectInfo
dbConnectInfo = getConnInfo <$> parseConfigFile "./database.cfg"

-- | AbstractDatabaseProvider that using given migration function performs
-- | given action.
abstractDatabaseProvider migration action = do
    dbInfo <- dbConnectInfo
    withMySQLPool dbInfo connCnt $ \pool ->
        flip runSqlPersistMPool pool $ do
            runMigration migration
            action
