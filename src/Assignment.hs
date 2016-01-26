{-# LANGUAGE TemplateHaskell #-}

module Assignment where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Text           (Text, unpack)
import           Data.Time
import           Data.Typeable
import           Database.Persist.TH
import           System.Directory
import           System.FilePath


-- | Constants
-- | Root directory of whole assignments data structure.
root :: String
root = "./"

-- | Default name used for storing configuration files.
configName :: String
configName = ".config"

-- | Default name used for storing assignment file.
assignmentFileName :: String
assignmentFileName = "Assignment.pdf"

-- | Custom Exception types
data NoSuchAssigmentException = NoSuchAssigmentException deriving (Show, Typeable)
instance Exception NoSuchAssigmentException

data AssigmentExistsException = AssigmentExistsException deriving (Show, Typeable)
instance Exception AssigmentExistsException

data NoSuchFileException = NoSuchFileException deriving (Show, Typeable)
instance Exception NoSuchFileException

data InvalidFileException = InvalidFileException deriving (Show, Typeable)
instance Exception InvalidFileException


-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String

-- | Academic year shorthand (e.g. 2015 for 2015/16)
type Year = Integer


-- | An assignment type
data Type = Homework | Exam | Project deriving (Show, Eq, Read) -- Read due to persistence

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
} deriving (Show, Read)


-- | An assignment descriptor
data Assignment = Assignment {
    year    :: Year,
    assType :: Type, -- | TODO: Nece type jer je keyword
    number  :: Int
} deriving (Show, Eq, Read) -- | Eq due to review module, Read due to persistence

derivePersistField "Assignment"

-- | Data structure modeling single submission.
data Submission = Submission {
    assignment       :: Assignment,
    userId           :: UserIdentifier,
    fileNames        :: [String], -- files that are uploaded (inside dir)
    lastModification :: UTCTime
} deriving (Show)


-- | Lists the user identifiers for submissions made for an assignment
listSubmissions :: Assignment -> IO [UserIdentifier]
listSubmissions a = filter f <$> dirContents (assignmentToPath a)
    where f = flip notElem [assignmentFileName, configName]


-- | Views a single submission in detail
getSubmission :: Assignment -> UserIdentifier -> IO Submission
getSubmission a uid = do
    let path = assignmentToPath a </> uid
    exists <- doesDirectoryExist path
    unless exists $ throw NoSuchFileException

    files <- dirContents path
    time  <- getCurrentTime
    return $ Submission a uid files time



-- | Creates a new assignment from Assignment, configuration and PDF file
-- | The PDF file should be copied, moved or symlinked so that it is
-- | accessible from the assignment directory.
createAssignment :: Assignment -> Configuration -> FilePath -> IO ()
createAssignment a cfg pdfPath = do
    testAssignmentAlreadyExist a
    pdfValid <- doesFileExist pdfPath

    unless pdfValid $ throw NoSuchFileException
    let path = assignmentToPath a

    createDirectoryIfMissing True path
    writeFile (path </> configName) $ show cfg
    copyFile pdfPath (path </> assignmentFileName)


-- | Gets the configuration object for an assignment
getConfiguration :: Assignment -> IO Configuration
getConfiguration a = do
    testExistAssignment a
    read <$> readFile (assignmentToPath a </> configName)


-- | Given a solution file body, adds a solution directory/file to the
-- | directory structure of an assignment. It will indicate an error
-- | (using Maybe, exceptions, or some other mechanism) if the file is
-- | not in a defined permitted list. It will override already made
-- | submissions.
-- | Added UserIdentifier as parameter!!!
-- | Invalid input is exception
-- | Assignment -> File Body -> File Name -> Error indication (?)
upload :: Assignment -> UserIdentifier -> Text -> String -> IO Submission
upload a uid txt fileName = do
    testExistAssignment a

    cfg <- getConfiguration a
    when (fileName `notElem` files cfg) $ throw InvalidFileException

    let subPath = assignmentToPath a </> uid
    createDirectoryIfMissing True subPath
    writeFile (subPath </> fileName) (unpack txt)

    getSubmission a uid

-- | Lists the files contained in a submission
listFiles :: Submission -> IO [FilePath]
listFiles = dirContents . getSubmissionPath


-- | Computes a file path for a submission
getSubmissionPath :: Submission -> FilePath
getSubmissionPath sub = lower $ assignmentToPath (assignment sub) </> userId sub



-- | Util functions
-------------------------------------------------------------------

--| Function checks existence of directory for given assignment.
existsAssigmentDir :: Assignment -> IO Bool
existsAssigmentDir a =  doesDirectoryExist $ assignmentToPath a

-- | Function that 'tests' if assignment already exists. In case it already exists
-- | throws adequate exception.
testAssignmentAlreadyExist :: Assignment -> IO ()
testAssignmentAlreadyExist a = do
    exists <- existsAssigmentDir a
    when exists $ throw AssigmentExistsException
    return ()

-- | Function that 'tests' if assignment exists. In case it doesn't exist
-- | throws adequate exception.
testExistAssignment :: Assignment -> IO ()
testExistAssignment a = do
    exists <- existsAssigmentDir a
    unless exists $ throw NoSuchAssigmentException
    return ()


-- | Function that determines path on file system for given assignment.
assignmentToPath :: Assignment -> FilePath
assignmentToPath a = lower $ root </> show (year a) </> show (assType a) </> show (number a)

-- | Function converts string to lower case.
lower :: String -> String
lower = map toLower

-- | Function that list contents of given directory path without '.' and '..'.
dirContents :: FilePath -> IO [FilePath]
dirContents path = filter (`notElem` [".", ".."]) <$> getDirectoryContents path


-----------------------------------------------------------------------------------
-- Test conf
dummyConf :: IO Configuration
dummyConf = do
    tm <- getCurrentTime
    return $ Configuration tm tm tm ["a", "b"] 1 3 2
