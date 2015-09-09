module Main where

import Types
import UI
import IO
import Pretty
import CalcNext

-- base
import System.IO ( hSetBuffering, stdout, BufferMode(..) )

-- time
import Data.Time ( getCurrentTime )

-- cmdargs
import System.Console.CmdArgs ( cmdArgs )

-- text
import Data.Text ( pack )


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- cmdArgs ui
    case input of
        Add t ts -> do
            let descr = pack (unwords (t:ts))
            now <- getCurrentTime
            Tasks tasks <- readTasks
            writeTasks (Tasks (Task descr [(now, Created)] [] NotDoneYet : tasks))
        Next -> do
            tasks <- readTasks
            case calcNext tasks of
                Nothing -> putStrLn "Nothing to do! (Not sure if that's a good thing or a bad thing)"
                Just next -> do
                    putStrLn (renderWide next)
                    putStrLn ""
                    cmd <- getNextCmd
                    now <- getCurrentTime
                    case applyNextCmd now cmd next tasks of
                        Nothing -> fail "Something Went Wrong (TM)"
                        Just tasks' -> writeTasks tasks'
        List -> do
            tasks <- readTasks
            putStrLn (renderWide tasks)

getNextCmd :: IO NextCmd
getNextCmd = do
    putStr "What do you want to do? (done, defer, split) -- "
    line <- getLine
    case line of
        "done"  -> return MarkAsDone
        "defer" -> return Defer
        "split" -> do
            putStrLn "Enter a list of subtasks. Terminate by entering a single dot."
            children <- getChildren 1
            if null children
                then fail "No subtasks given. Aborting."
                else return (Split (map pack children))
        _ -> getNextCmd

getChildren :: Int -> IO [String]
getChildren n = do
    putStr ("Part #" ++ show n ++ ": ")
    line <- getLine
    case line of
        ""  -> getChildren n
        "." -> return []
        _   -> (line:) <$> getChildren (n+1)
