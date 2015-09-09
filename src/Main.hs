module Main where

import Types
import UI
import IO
import Pretty

-- time
import Data.Time ( getCurrentTime )

-- cmdargs
import System.Console.CmdArgs ( cmdArgs )

-- text
import Data.Text ( pack )


main :: IO ()
main = do
    input <- cmdArgs ui
    print input
    case input of
        Add t ts -> do
            let descr = pack (unwords (t:ts))
            now <- getCurrentTime
            Tasks tasks <- readTasks
            writeTasks (Tasks (Task descr [(now, Created)] [] Active : tasks))
        Next -> do
            tasks <- readTasks
            putStrLn (renderWide tasks)
        List -> do
            tasks <- readTasks
            putStrLn (renderWide tasks)
