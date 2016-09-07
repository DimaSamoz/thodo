-- | Handler for the 'init' command.
module Commands.Init.Handler
    ( handleInitCommand
    ) where

import Types
import Printing
import Util
import Commands.Common
import Options.Applicative
import System.FilePath
import System.IO
import System.Directory
import Data.Time


handleInitCommand :: IO ()
handleInitCommand = do
    hSetBuffering stdout NoBuffering
    putStr "Please enter your name: "
    name <- getLine
    currentTime <- getCurrentTime
    putStrLn "Creating your to-do list file..."
    createDirectoryIfMissing False "todo"
    let filename = "todo" </> "list.txt"
        date = utctDay currentTime
        emptyTodoList = initTodoList name date
    writeToFile filename emptyTodoList
    putStrLn "File created."
