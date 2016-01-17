module User where

-- | A user identifier (not DB id) like a username or JMBAG
type UserIdentifier = String

-- | The user's role in the course
data Role = Student Integer -- Academic Year shorthand (2015 for 2015/16)
    | TA Integer Integer -- AY shorthand range (inclusive)
    | Professor deriving (Eq, Ord, Show)



-- | A user (the definition can be bigger)
data User = User {
    identifier :: UserIdentifier,
    email :: String,
    pwdHash :: String,
    role :: Role
    } deriving (Eq, Show)


-- | Takes a user identifier, e-mail, password and role.
-- | Performs password hashing and stores the user into the
-- | database, returning a filled User. If creating it fails (e.g.
-- | the user identifier is already taken), throws an appropriate
-- | exception.
createUser :: UserIdentifier -> String -> String -> Role -> IO User
createUser = undefined

-- | Updates a given user. Identifies it by the UserIdentifier (or
-- | maybe database id field, if you added it) in the User and overwrites
-- | the DB entry with the values in the User structure. Throws an
-- | appropriate error if it cannot do so; e.g. the user does not exist.
updateUser :: User -> IO ()
updateUser = undefined

-- | Deletes a user referenced by identifier. If no such user or the
-- | operation fails, an appropriate exception is thrown.
deleteUser :: UserIdentifier -> IO ()
deleteUser = undefined

-- | Lists all the users
listUsers :: IO [User]
listUsers = undefined

-- | Lists all users in a given role
listUsersInRole :: Role -> IO [User]
listUsersInRole = undefined

-- | Fetches a single user by identifier
getUser :: UserIdentifier -> IO User
getUser = undefined

-- | Checks whether the user has a role of AT LEAST X in a given academic
-- | year.
isRoleInYear :: User -> Role -> Integer -> Bool
isRoleInYear = undefined
