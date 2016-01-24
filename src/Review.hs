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

module Review where

import           Assignment
import           Control.Applicative          (pure, (<$>))
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (MonadLogger, monadLoggerLog)
import           Data.Pool
import           Data.Random                  (runRVar)
import           Data.Random.Extras           (sample)
import           Data.Random.List             (shuffle)
import           Data.Random.RVar
import           Data.Random.Source.DevRandom
import           Data.Text                    (Text)
import           Data.Typeable
import           Data.Word
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import           DatabaseAccess
import           DBConfig
import           ReviewRole


data ReviewAssignmentExistsException = ReviewAssignmentExistsException deriving (Show, Typeable)
instance Exception ReviewAssignmentExistsException



share [mkPersist sqlSettings, mkMigrate "migrateReviewAssignments"] [persistLowerCase|
ReviewAssignment
    reviewer UserIdentifier maxlen=10
    reviewee UserIdentifier maxlen=10
    role ReviewRole
    assignment Assignment
    deriving Show Eq
|]


databaseProviderReviews :: SqlPersistM a -> IO a
databaseProviderReviews action = abstractDatabaseProvider migrateReviewAssignments action


-- | Takes an Assignment, a list of reviewer identifiers and a
-- | list of reviewee identifiers and assigns N reviewees for each
-- | reviewer. It makes sure that a user never reviews themselves.
-- | The reviewer is assigned the reviews with the provided role.
assignNReviews :: Assignment -> [UserIdentifier]
    -> [UserIdentifier]
    -> Int
    -> ReviewRole
    -> IO [ReviewAssignment]
assignNReviews _ [] _ _ _ = return []
assignNReviews a (x:xs) ys n r = liftM2 (++) (createReviewList' a x ys n r) (assignNReviews a xs ys n r)


createReviewList' :: Assignment -> UserIdentifier -> [UserIdentifier] -> Int -> ReviewRole -> IO [ReviewAssignment]
createReviewList' a x ys n = createReviewList a x (createRandomList (removeIdList x ys) n) n

-- | Creates list of review assignments for given reviewer and list of reviees.
createReviewList :: Assignment -> UserIdentifier -> RVar [UserIdentifier] -> Int -> ReviewRole -> IO [ReviewAssignment]
createReviewList a x ys n r = do
    list <- runRVar ys DevRandom
    forM list $ \y -> return (ReviewAssignment x y r a)

-- | Removes given id from list.
removeIdList :: UserIdentifier -> [UserIdentifier] -> [UserIdentifier]
removeIdList x = filter (/= x)

-- | Creates random list of user identifiers. How should we handle
-- | case when n exceeds length of list.
createRandomList :: [UserIdentifier] -> Int -> RVar [UserIdentifier]
createRandomList ids n = sample n ids


shuffleList :: [a] -> IO [a]
shuffleList xs = runRVar (shuffle xs) DevRandom


-- | Takes an assignment, a list of reviewers and reviewees and a
-- | role. Assigns revieews to reviewers pseudorandomly until the
-- | list of revieews is exhausted. For N reviewers and M
-- | reviewees, ensures no reviewer gets less than
-- | floor (M / N) or more than ceil (M / N) reviews.
-- | Should NOT always assign more reviews to users listed
-- | at the beginning of the reviewer list.
assignReviews :: Assignment -> [UserIdentifier]
    -> [UserIdentifier]
    -> ReviewRole
    -> IO [ReviewAssignment]
assignReviews a xs ys r = do
    reviewers <- shuffleList xs
    reviews <-  shuffleList ys

    let pairs = zip (cycle reviewers) reviews
        unique = filter (uncurry (/=)) pairs
        same = map fst $ filter (uncurry (==)) pairs
        extra = zip same (tail $ cycle same)
        -- | TODO: fix for 1 reviewer [] crash
        revs = unique ++ extra

    return $ map makeRev revs
    where
        makeRev t = ReviewAssignment (fst t) (snd t) r a


-- | Stores a list of review assignments into a database or
-- | file system.
storeAssigments :: [ReviewAssignment] -> IO ()
storeAssigments xs = forM_ xs $ \x -> databaseProviderReviews $ do
        existing <- selectList
            [ReviewAssignmentAssignment ==. reviewAssignmentAssignment x,
            ReviewAssignmentReviewer ==. reviewAssignmentReviewer x,
            ReviewAssignmentReviewee ==. reviewAssignmentReviewee x,
            ReviewAssignmentRole ==. reviewAssignmentRole x] []
        unless (null existing) $ throw ReviewAssignmentExistsException
        void (insert x)


-- | Retrieves all ReviewAssignments for an Assignment from
-- | a database or file system.
assignedReviews :: Assignment -> IO [ReviewAssignment]
assignedReviews a = databaseProviderReviews $ do
    reviews <- selectList [ReviewAssignmentAssignment ==. a] []
    liftIO $ return $ map unwrapEntity reviews


-- | Retrieves all ReviewAssignments for an Assignment and
-- | a UserIdentifier, i.e. all the reviews for that assigment
-- | the user has to perform.
assignmentsBy :: Assignment -> UserIdentifier -> IO [ReviewAssignment]
assignmentsBy a r = databaseProviderReviews $ do
    reviews <- selectList [ReviewAssignmentAssignment ==. a, ReviewAssignmentReviewer ==. r] []
    liftIO $ return $ map unwrapEntity reviews


-- | Retrieves all ReviewAssignments for an Assignment and
-- | a UserIdentifier, i.e. all the reviews for that assignment
-- | where the user is a reviewee.
assignmentsFor :: Assignment -> UserIdentifier -> IO [ReviewAssignment]
assignmentsFor a r = databaseProviderReviews $ do
    reviews <- selectList [ReviewAssignmentAssignment ==. a, ReviewAssignmentReviewee ==. r] []
    liftIO $ return $ map unwrapEntity reviews

-- | Utility function for unwrapping data from Entity context.
unwrapEntity :: Entity a -> a
unwrapEntity (Entity id x) = x


-- | Dummy assignments.
ass1 :: Assignment
ass1 = Assignment 2010 Homework 10

ass2 :: Assignment
ass2 = Assignment 2015 Homework 11

-- | Dummy identifiers.
reviewers :: [UserIdentifier]
reviewers = ["0", "1", "2", "3", "4"]

reviewees :: [UserIdentifier]
reviewees = ["a", "b", "c", "d", "e", "0", "1", "2", "3", "4"]


rassignments :: IO [ReviewAssignment]
rassignments = assignNReviews ass1 reviewers reviewees 3 Student

store :: IO [ReviewAssignment] -> IO ()
store ass = do
    rass <- ass
    storeAssigments rass
