-- | Handler for the 'what' command.
module Commands.What.Handler
    ( handleWhatCommand
    , printGroups
    ) where

import Types
import Printing
import Parsing
import TodoLenses
import Commands.Common
import Control.Exception

-- | Handler for the 'what' command.
handleWhatCommand :: Command -> IO ()
handleWhatCommand (What categories) = do
    todoList <- parseWith parseTodoList <$> readFile "todo/list.txt"
    case todoList of
        Right list -> printGroups list categories
        Left err -> throwIO (ParseException err)

-- | Prints the groups in the list having the given categories.
printGroups :: TodoList -> [Category] -> IO ()
printGroups list cats = mapM_ (printStrings . showGroup) groups
    where groups = map (getGroupByCategory list) cats
