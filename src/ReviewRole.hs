{-# LANGUAGE TemplateHaskell #-}

module ReviewRole where

import           Database.Persist.TH


-- | A user’s role or authorization level as a reviewer
data ReviewRole = Student | Staff deriving (Eq, Ord, Show, Read)

derivePersistField "ReviewRole"
