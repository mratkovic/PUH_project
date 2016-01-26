import Assignment
import Review
import ReviewResults
import DBConfig


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
rev_as = assignNReviews ass1 reviewers reviewees 3 Student

storeAs :: IO [ReviewAssignment] -> IO ()
storeAs ass = do
    rass <- ass
    storeAssigments rass

storeReviews :: IO [ReviewAssignment] -> IO ()
storeReviews xs = do
    ys <- xs
    forM ys $ \x -> databaseProviderReviewResults $
        insert (Review x 3.0 "aaa")
    return ()



main :: IO ()
main = undefined
