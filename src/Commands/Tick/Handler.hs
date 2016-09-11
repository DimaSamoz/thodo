-- | Handler for the 'tick' command.
module Commands.Tick.Handler
    ( handleTickCommand
    ) where

import Types
import Util
import Parsing
import TodoLenses
import Commands.What.Handler
import Commands.Common
import Options.Applicative
import Text.Read (readMaybe)
import Data.Char (ord)
import Control.Monad (unless)
import Control.Exception (throwIO)

handleTickCommand :: Command -> IO ()
handleTickCommand (Tick category) = do
    todoList <- parseWith parseTodoList <$> readFile "todo/list.txt"
    case todoList of
        Right oldList -> loop oldList category
        Left err -> throwIO (ParseException err)
    where loop list cat = do
            printGroups list [cat]
            input <- prompt "\nTask or item to be ticked: "
            unless (input == "") $ do
                let newList = case input of
                        [taskIxS] -> do
                            let taskIxM = readMaybe [taskIxS] :: Maybe Int
                            case taskIxM of
                                Just taskIx -> tickTask list category (taskIx - 1)
                                Nothing -> list
                        [taskIxS, itemIxC] -> do
                            let taskIxM = readMaybe [taskIxS] :: Maybe Int
                                itemIx = ord itemIxC - ord 'a'
                            case taskIxM of
                                Just taskIx -> tickItem list category (taskIx - 1) itemIx
                                Nothing -> list
                        _ -> list
                updateTodoList newList
                loop newList cat
