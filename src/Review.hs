module Review where

import Data.Text (Text)
import Assignment
import Data.Random (runRVar)
import Data.Random.Source.DevRandom
import Data.Random.Extras (sample)
import Data.Random.RVar
import Control.Monad

-- | A user’s role or authorization level as a reviewer
data Role = Student | Staff deriving (Eq, Ord, Show)


-- | A review assignment representation
data ReviewAssignment = ReviewAssignment {
    reviewer   :: UserIdentifier,
    reviewee   :: UserIdentifier,
    role       :: Role,
    assignment :: Assignment
} deriving (Eq, Show)


-- | A finished review
data Review = Review {
    reviewAssignment :: ReviewAssignment,
    score            :: Double,
    text             :: Text
} deriving Show



-- | Takes an Assignment, a list of reviewer identifiers and a
-- | list of reviewee identifiers and assigns N reviewees for each
-- | reviewer. It makes sure that a user never reviews themselves.
-- | The reviewer is assigned the reviews with the provided role.
assignNReviews :: Assignment -> [UserIdentifier]
    -> [UserIdentifier]
    -> Int
    -> Role
    -> IO [ReviewAssignment]

assignNReviews _ [] _ _ _ = return []
assignNReviews a (x:xs) ys n r = liftM2 (++) (createReviewList' a x ys n r) (assignNReviews a xs ys n r)

createReviewList' :: Assignment -> UserIdentifier -> [UserIdentifier] -> Int -> Role -> IO [ReviewAssignment]
createReviewList' a x ys n r = createReviewList a x (createRandomList (removeIdList x ys) n) n r

-- | Creates list of review assignments for given reviewer and list of reviees.
createReviewList :: Assignment -> UserIdentifier -> RVar [UserIdentifier] -> Int -> Role -> IO [ReviewAssignment]
createReviewList a x ys n r = do
    list <- runRVar ys DevRandom
    rs <- forM list $ \y -> do
        return (ReviewAssignment x y r a)
    return rs

-- | Removes given id from list.
removeIdList :: UserIdentifier -> [UserIdentifier] -> [UserIdentifier]
removeIdList x = filter (\y -> y /= x) 

-- | Creates random list of user identifiers. How should we handle 
-- | case when n exceeds length of list.
createRandomList :: [UserIdentifier] -> Int -> RVar [UserIdentifier]
createRandomList ids n = sample n ids

-- | Takes an assignment, a list of reviewers and reviewees and a
-- | role. Assigns revieews to reviewers pseudorandomly until the
-- | list of revieews is exhausted. For N reviewers and M
-- | reviewees, ensures no reviewer gets less than
-- | floor (M / N) or more than ceil (M / N) reviews.
-- | Should NOT always assign more reviews to users listed
-- | at the beginning of the reviewer list.
assignReviews :: Assignment -> [UserIdentifier]
    -> [UserIdentifier]
    -> Role
    -> IO [ReviewAssignment]
assignReviews = undefined

-- | Returns random user identifier from given list of
-- | user identifiers.
randomId :: [UserIdentifier] -> UserIdentifier
randomId = undefined

-- | Removes given user identifier from list of user
-- | identifiers.
removeId :: UserIdentifier -> [UserIdentifier] -> UserIdentifier
removeId = undefined

-- | Stores a list of review assignments into a database or
-- | file system.
storeAssigments :: [ReviewAssignment] -> IO ()
storeAssigments = undefined


-- | Retrieves all ReviewAssignments for an Assignment from
-- | a database or file system.
assignedReviews :: Assignment -> IO [ReviewAssignment]
assignedReviews = undefined


-- | Retrieves all ReviewAssignments for an Assignment and
-- | a UserIdentifier, i.e. all the reviews for that assigment
-- | the user has to perform.
assignmentsBy :: Assignment -> UserIdentifier -> IO [ReviewAssignment]
assignmentsBy = undefined


-- | Retrieves all ReviewAssignments for an Assignment and
-- | a UserIdentifier, i.e. all the reviews for that assignment
-- | where the user is a reviewee.
assignmentsFor :: Assignment -> UserIdentifier -> IO [ReviewAssignment]
assignmentsFor = undefined


-- | Completes a review assignment and stores the result in a
-- | file system or database.
saveReview :: Review -> IO ()
saveReview = undefined


-- | Loads all the completed review results for an assignment
reviews :: Assignment -> IO [Review]
reviews = undefined



-- | Loads all the completed review results for an assignment
-- | that were performed by a user.
reviewsBy :: Assignment -> UserIdentifier -> IO [Review]
reviewsBy = undefined



-- | Loads all the completed review results for an assignment
-- | where the user’s code was being reviewed.
reviewsFor :: Assignment -> UserIdentifier -> IO [Review]
reviewsFor = undefined
