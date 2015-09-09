module IO where

import Types

-- directory
import System.Directory ( createDirectoryIfMissing, doesFileExist, getHomeDirectory )

-- filepath
import System.FilePath ( (</>) )

-- bytestring
import Data.ByteString.Lazy as BS ( readFile, writeFile )

-- aeson
import Data.Aeson ( decode )

-- aeson-pretty
import Data.Aeson.Encode.Pretty ( encodePretty )


getLoc :: IO FilePath
getLoc = do
    home <- getHomeDirectory
    createDirectoryIfMissing False (home </> ".task-chop")
    let file = home </> ".task-chop/tasks.json"
    return file

readTasks :: IO Tasks
readTasks = do
    file <- getLoc
    ohItDoes <- doesFileExist file
    if ohItDoes
        then do
            txt <- BS.readFile file
            case decode txt of
                Nothing -> fail $ unlines
                    [ "Malformed JSON :("
                    , "You may need to delete the following file and start again:"
                    , file
                    ]
                Just tasks -> return tasks
        else return (Tasks [])

writeTasks :: Tasks -> IO ()
writeTasks tasks = do
    file <- getLoc
    BS.writeFile file (encodePretty tasks)
