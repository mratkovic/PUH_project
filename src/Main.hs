{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import Control.Monad.Logger (MonadLogger, monadLoggerLog)
import Control.Applicative  (pure)

instance MonadLogger IO where
    monadLoggerLog _ _ _ = pure $ pure ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show

BlogPost
    title String
    authorId PersonId
    deriving Show
|]



connInfo :: ConnectInfo

connInfo = defaultConnectInfo {
    connectHost     = "localhost",
    connectPort     = 3306,
    connectUser     = "root",
    connectPassword = "toor",
    connectDatabase = "test"
}

connCnt :: Int
connCnt = 10

main :: IO ()
main = withMySQLPool connInfo connCnt $ \pool -> do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        johnId <- insert $ Person "John Doe" $ Just 35
        janeId <- insert $ Person "Jane Doe" Nothing

        insert $ BlogPost "My fr1st p0st" johnId
        insert $ BlogPost "One more for good measure" johnId

        oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
        liftIO $ print (oneJohnPost :: [Entity BlogPost])

        john <- get johnId
        liftIO $ print (john :: Maybe Person)

        delete janeId
        deleteWhere [BlogPostAuthorId ==. johnId]
