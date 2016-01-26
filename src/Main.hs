{-# LANGUAGE OverloadedStrings          #-}

import Assignment
import Review
import ReviewResults
import qualified User as Usr
import DBConfig
import Role 
import qualified  ReviewRole as RRole

import Control.Monad
import Data.Time.Clock
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.TH
import           Control.Monad.IO.Class (liftIO)


dummyConf :: IO Configuration
dummyConf = do
    tm <- getCurrentTime
    return $ Configuration tm tm tm ["a", "b"] 1 3 2

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


rev_as :: IO [ReviewAssignment]
rev_as = assignNReviews ass1 reviewers reviewees 3 RRole.Student

rev_as2 :: IO [ReviewAssignment]
rev_as2 = assignReviews ass2 reviewers reviewees RRole.Student


storeAs :: IO [ReviewAssignment] -> IO ()
storeAs ass = do
    rass <- ass
    storeAssigments rass

storeReviews :: IO [ReviewAssignment] -> IO ()
storeReviews xs = do
    ys <- xs
    forM ys $ \x -> databaseProviderReviewResults $
        liftIO $ saveReview (Review x 50.0 "aaa")
    return ()

storeReviews2 :: IO [ReviewAssignment] -> IO ()
storeReviews2 xs = do
    ys <- xs
    forM ys $ \x -> databaseProviderReviewResults $
        liftIO $ saveReview (Review x 3.0 "aaa")
    return ()


createDummyAssignment :: IO ()
createDummyAssignment = do
	cnf <- dummyConf
	createAssignment ass1 cnf "/home/dumpram/PUH_project/src/Role.hs"
	createAssignment ass2 cnf "/home/dumpram/PUH_project/src/User.hs"

main :: IO ()
main = undefined
