module DBConfig where

import Data.Char
import Data.List
import Data.List.Split
import Control.Monad (liftM)

parseConfigFile :: String -> IO DBConfig
parseConfigFile path = parseConfig <$> readFile path

parseConfig :: String -> DBConfig
parseConfig = foldr addConfigValue defaultConfig . clean . lines
    where clean = filter (not . flip any ["#", ";", "", " "] . (==) . take 1)

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

data DBConfig = DBConfig
    { hostname  :: String
    , port      :: Int
    , username  :: String
    , password  :: String
    , database  :: String
    } deriving (Show)

defaultConfig :: DBConfig
defaultConfig = DBConfig "localhost" 3306 "root" "" "test"


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
