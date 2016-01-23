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

module ReviewResults where
import Data.Text (Text)
import Assignment
import Review
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import Control.Monad.IO.Class  (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateReviewResults"] [persistLowerCase|
Review
    reviewAssignment ReviewAssignment
    score Double
    text Text
    deriving Show
|]

databaseProviderResults :: SqlPersistM a -> IO a
databaseProviderResults action = do
    dbInfo <- dbConnectInfo
    withMySQLPool dbInfo connCnt $ \pool ->
        flip runSqlPersistMPool pool $ do
            runMigration migrateReviewResults
            action

-- | Completes a review assignment and stores the result in a
-- | file system or database.
saveReview :: Review -> IO ()
saveReview x = databaseProviderResults $ do 
	insert x
	return ()

-- | Loads all the completed review results for an assignment
reviews :: Assignment -> IO [Review]
reviews a = undefined

-- | Loads all the completed review results for an assignment
-- | that were performed by a user.
reviewsBy :: Assignment -> UserIdentifier -> IO [Review]
reviewsBy = undefined

-- | Loads all the completed review results for an assignment
-- | where the user’s code was being reviewed.
reviewsFor :: Assignment -> UserIdentifier -> IO [Review]
reviewsFor = undefined
