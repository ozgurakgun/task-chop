module IO
    ( getContexts
    , getCurrentContext
    , setCurrentContext
    , readTasks
    , writeTasks
    ) where

import Types

-- base
import Data.List ( nub )

-- directory
import System.Directory ( createDirectoryIfMissing, doesFileExist, getHomeDirectory, getDirectoryContents )

-- filepath
import System.FilePath ( (</>), (<.>), splitExtension )

-- bytestring
import qualified Data.ByteString.Lazy as BS ( readFile, writeFile )

-- aeson
import Data.Aeson ( decode )

-- aeson-pretty
import Data.Aeson.Encode.Pretty ( encodePretty )


getDir :: IO FilePath
getDir = do
    home <- getHomeDirectory
    let dir = home </> ".task-chop"
    createDirectoryIfMissing False dir
    return dir

getContexts :: IO [String]
getContexts = do
    dir <- getDir
    allFiles <- getDirectoryContents dir
    return $ nub $ "default" : [ base | (base, ".json") <- map splitExtension allFiles ]

getCurrentContext :: IO String
getCurrentContext = do
    dir <- getDir
    let current_context = dir </> "current_context.txt"
    ohItDoes <- doesFileExist current_context
    if ohItDoes
        then readFile (dir </> "current_context.txt")
        else return "default"

getCurrentContextFile :: IO FilePath
getCurrentContextFile = do
    dir  <- getDir
    curr <- getCurrentContext
    return (dir </> curr <.> "json")

setCurrentContext :: String -> IO ()
setCurrentContext ctxt = do
    dir <- getDir
    writeFile (dir </> "current_context.txt") ctxt

readTasks :: IO Tasks
readTasks = do
    file <- getCurrentContextFile
    ohItDoes <- doesFileExist file
    if ohItDoes
        then do
            txt <- BS.readFile file
            case decode txt of
                Nothing -> error $ unlines
                    [ "Malformed JSON :("
                    , "You may need to delete the following file and start again:"
                    , file
                    ]
                Just tasks -> return tasks
        else return (Tasks [])

writeTasks :: Tasks -> IO ()
writeTasks tasks = do
    file <- getCurrentContextFile
    BS.writeFile file (encodePretty tasks)
