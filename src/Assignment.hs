module Assignment where
import Data.Text
import Data.Time


-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String

-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer


-- | An assignment type
data Type = Homework | Exam | Project deriving (Show, Eq)

-- | A an assignment configuration data structure
-- | Uses Data.Time.UTCTime
-- | If files is an empty list, ANY number of files are OK
data Configuration = Configuration {
    published    :: UTCTime, -- When to publish
    deadline     :: UTCTime, -- Submission deadline
    lateDeadline :: UTCTime, -- Late submission deadline
    files        :: [String], -- File names to expect
    minScore     :: Double, -- Minimum achievable
    maxScore     :: Double, -- Maximum achievable
    required     :: Double -- Score req to pass
} deriving Show


-- | An assignment descriptor
data Assignment = Assignment {
    year    :: Year,
    assType :: Type, -- | TODO: Nece type jer je keyword
    number  :: Int
} deriving (Show, Eq) -- | Eq due to review module



data Submission = Submission

-- | Lists the user identifiers for submissions made for an assignment
listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions = undefined

-- | Views a single submission in detail
getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission = undefined

-- | Creates a new assignment from Assignment, configuration and PDF file
-- | The PDF file should be copied, moved or symlinked so that it is
-- | accessible from the assignment directory.
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment = undefined

-- | Gets the configuration object for an assignment
getConfiguration :: Assignment -> IO Configuration
getConfiguration = undefined

-- | Given a solution file body, adds a solution directory/file to the
-- | directory structure of an assignment. It will indicate an error
-- | (using Maybe, exceptions, or some other mechanism) if the file is
-- | not in a defined permitted list. It will override already made
-- | submissions.
-- | Assignment -> File Body -> File Name -> Error indication (?)
upload :: Assignment -> Text -> String -> IO (Maybe Submission)
upload = undefined

-- | Lists the files contained in a submission
listFiles :: Submission -> IO [FilePath]
listFiles = undefined

-- | Computes a file path for a submission
getSubmissionPath :: Submission -> FilePath
getSubmissionPath = undefined
