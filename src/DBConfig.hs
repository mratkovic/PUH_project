module DBConfig where

import           Data.Char

-- | Function that parses file as DBConfig.
parseConfigFile :: String -> IO DBConfig
parseConfigFile path = parseConfig <$> readFile path

-- | Function that parses DBConfig from given string.
parseConfig :: String -> DBConfig
parseConfig = foldr addConfigValue defaultConfig . clean . lines
    where clean = filter (not . flip any ["#", ";", "", " "] . (==) . take 1)

-- | Function that parses string and if valid stores it to database config.
addConfigValue :: String -> DBConfig -> DBConfig
addConfigValue raw config = case key of
    "hostname"  -> config {hostname = values}
    "port"      -> config {port     = read values :: Int}
    "username"  -> config {username = values}
    "password"  -> config {password = values}
    "database"  -> config {database = values}
    _           -> config
    where (k, vs) = span (/= ':') raw
          key = map toLower $ trim k
          values = trim $ tail vs

-- | Data type that stores database connection credentials.
data DBConfig = DBConfig
    { hostname :: String
    , port     :: Int
    , username :: String
    , password :: String
    , database :: String
    } deriving (Show)

defaultConfig :: DBConfig
defaultConfig = DBConfig "localhost" 3306 "root" "" "test"

-- | Function strips given string of leading and trailing white spaces.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
