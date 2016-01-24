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
import Data.Typeable
import Assignment
import Control.Exception
import Review
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH
import Control.Monad.IO.Class  (liftIO)
import Control.Monad
import DatabaseAccess

data InvalidScoreException = InvalidScoreException deriving (Show, Typeable)
instance Exception InvalidScoreException


data ReviewExistsException = ReviewExistsException deriving (Show, Typeable)
instance Exception ReviewExistsException


share [mkPersist sqlSettings, mkMigrate "migrateReviewResults"] [persistLowerCase|
Review
    reviewAssignment ReviewAssignment
    score Double
    text Text
    deriving Show
|]

databaseProviderReviewResults :: SqlPersistM a -> IO a
databaseProviderReviewResults action = abstractDatabaseProvider migrateReviewResults action

-- | Completes a review assignment and stores the result in a
-- | file system or database.
saveReview :: Review -> IO ()
saveReview x = databaseProviderReviewResults $ do
    let as    = reviewAssignmentAssignment $ reviewReviewAssignment x
        score = reviewScore x

    conf <- liftIO $ getConfiguration as
    existing <- liftIO $ reviewsForAssigments [reviewReviewAssignment x]
    unless (null existing) $ throw ReviewExistsException
    void (insert x)



    -- when (score > maxScore conf || score < minScore conf) $ throw InvalidFileException
    -- f <- insertBy x
    -- case f of
    --     Left (Entity _ _) -> throw ReviewExistsException
    --     Right _           -> liftIO $ return ()

-- | Loads all the completed review results for an assignment
reviews :: Assignment -> IO [Review]
reviews a = assignedReviews a >>= reviewsForAssigments

-- | Loads all the completed review results for an assignment
-- | that were performed by a user.
reviewsBy :: Assignment -> UserIdentifier -> IO [Review]
reviewsBy a uid = assignmentsBy a uid >>= reviewsForAssigments


-- | Loads all the completed review results for an assignment
-- | where the user’s code was being reviewed.
reviewsFor :: Assignment -> UserIdentifier -> IO [Review]
reviewsFor a uid = assignmentsFor a uid >>= reviewsForAssigments

-- | Utility method that fetches reviews for assignments passed
-- | as input parameter.
reviewsForAssigments :: [ReviewAssignment] -> IO [Review]
reviewsForAssigments as = databaseProviderReviewResults $ do
    reviews <- selectList [ReviewReviewAssignment <-. as] []
    liftIO . return $ map unwrapEntity reviews

storeReviews :: IO [ReviewAssignment] -> IO ()
storeReviews xs = do
    ys <- xs
    forM ys $ \x -> databaseProviderReviewResults $
        insert (Review x 3.0 "aaa")
    return ()
