-- | Handler for the 'what' command.
module Commands.What.Handler
    ( handleWhatCommand
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
        Right list -> do
            let groups = map (getGroupByCategory list) categories
            mapM_ (printStrings . showGroup) groups
        Left err -> throwIO (ParseException err)
