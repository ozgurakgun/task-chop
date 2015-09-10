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
            let packed = pack (unwords (t:ts))
            now <- getCurrentTime
            Tasks tasks <- readTasks
            writeTasks (Tasks (Task packed [(now, Created)] [] NotDoneYet : tasks))
        Next -> do
            tasks <- readTasks
            case calcNext tasks of
                Nothing -> nothingToDo
                Just next -> do
                    putStrLn (renderWide (next { children = filterDone (children next) }))
                    putStrLn ""
                    cmd <- getNextCmd
                    now <- getCurrentTime
                    case applyNextCmd now cmd next tasks of
                        Nothing -> error "Something Went Wrong (TM)"
                        Just tasks' -> writeTasks tasks'
        List{} -> do
            curr <- getCurrentContext
            putStrLn $ "[Context: " ++ curr ++ "]"
            Tasks tasks0 <- readTasks
            let tasks =
                    if uiListAll input
                        then tasks0
                        else filterDone tasks0
            if null tasks
                then nothingToDo
                else putStrLn (renderWide (Tasks tasks))
        Context{} ->
            case uiContextTo input of
                [] -> do
                    ctxts <- getContexts
                    curr  <- getCurrentContext
                    let widest = maximum (map length ctxts)
                    putStrLn $ renderWide $ vcat
                        [ if c == curr
                            then padded widest c <+> pretty "(active)"
                            else pretty c
                        | c <- ctxts ]
                [ctxt] ->
                    setCurrentContext ctxt
                _ ->
                    error "Was expecting zero or one arguments, don't know what to do now. :("

nothingToDo :: IO ()
nothingToDo = putStrLn "Nothing to do! (Not sure if that's a good thing or a bad thing)"

getNextCmd :: IO NextCmd
getNextCmd = do
    putStr "What do you want to do? (done, defer, split) -- "
    line <- getLine
    case line of
        "done"  -> return MarkAsDone
        "defer" -> return Defer
        "split" -> do
            putStrLn "Enter a list of subtasks. Terminate by entering a single dot."
            children_ <- getChildren 1
            if null children_
                then error "No subtasks given. Aborting."
                else return (Split (map pack children_))
        _ -> getNextCmd

getChildren :: Int -> IO [String]
getChildren n = do
    putStr ("Part #" ++ show n ++ ": ")
    line <- getLine
    case line of
        ""  -> getChildren n
        "." -> return []
        _   -> (line:) <$> getChildren (n+1)
