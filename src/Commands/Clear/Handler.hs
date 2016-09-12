-- | Handler for the 'clear' command.
module Commands.Clear.Handler
    ( handleClearCommand
    ) where

import Types
import Util
import Parsing
import Printing
import TodoLenses
import Commands.Common
import Commands.Clear.Parser
import Options.Applicative
import Control.Exception (throwIO)
import Data.Char (toLower)

-- | Handler for the 'clear' command
handleClearCommand :: Command -> IO ()
handleClearCommand (Clear t) = do
    todoList <- parseWith parseTodoList <$> readFile "todo/list.txt"
    case todoList of
        Right oldList -> case t of
            Ticked -> do
                let (ticked, newList) = clearTicked oldList
                printStrings $ concatMap showTask (zip [1..] ticked)
                confirm <- prompt "\n Are you sure you want to clear the ticked tasks? (y/n) "
                case map toLower confirm of
                    "y" -> updateTodoList newList
                    _   -> putStrLn "No changes were made."
            All -> do
                let newList = clearAll oldList
                confirm <- prompt "\n Are you sure you want to clear the ticked tasks? (yes/no) "
                case map toLower confirm of
                    "yes" -> updateTodoList newList
                    _   -> putStrLn "No changes were made."
        Left err -> throwIO (ParseException err)
