-- | Handler for the 'add' command.
module Commands.Add.Handler
    ( TodoTask
    , AddItems
    , DayOfWeek
    , handleAddCommand
    ) where

import Types
import Util
import Parsing
import Printing
import TodoLenses
import Commands.Common
import Options.Applicative
import Data.Time hiding (parseTime)
import Data.Time.Calendar.OrdinalDate
import Control.Exception
import System.Directory

-- | Handler for the 'add' command.
handleAddCommand :: Command -> IO ()
handleAddCommand (Add task cat pri addItems dowM dateM timeM) = do
    items <- if addItems
        then putStrLn "Type in the items one by one, then press Enter twice when done:" >> askItems 'a'
        else return []
    newTask <- constructTask (task, items, cat, dowM, dateM, timeM, pri)
    todoList <- parseWith parseTodoList <$> readFile "todo/list.txt"
    case todoList of
        Right list -> do
            removeFile "todo/list.txt"      -- Old is removed to avoid race conditions.
            newList <- addTask list newTask
            writeToFile "todo/list.txt" newList
        Left e -> throwIO (ParseException e)

-- Ask for the list of items belonging to the task.
askItems :: Char -> IO [Item]
askItems c = do
    item <- prompt ("  " ++ [c] ++ ") ")
    if item == "" then return []
        else do
            rest <- askItems (succ c)
            return $ Item item False : rest

-- Construct a task from its parameters.
constructTask :: (TodoTask, [Item], Category, Maybe DayOfWeek, Maybe Day, Maybe TimeOfDay, Priority) -> IO Task
constructTask (task, items, cat, dowM, dateM, timeM, pri) = do
    deadline <- case dowM of
        Just dow -> dayOfWeekToDeadline cat dow timeM           -- Day of week is given
        Nothing -> case dateM of
            Just date -> return $ makeAbsDeadline date timeM    -- Date is given
            Nothing -> case cat of
                RelTime t -> return $ Rel t                     -- Category is given
                _ -> return None
    return Task {_desc = task, _items = items, _deadline = deadline, _priority = pri, _done = False}

-- Creates a deadline from the category, day of week, time of day and current date.
dayOfWeekToDeadline :: Category -> DayOfWeek -> Maybe TimeOfDay -> IO Deadline
dayOfWeekToDeadline (Custom _) _ _ = throwIO AmbiguousDay
dayOfWeekToDeadline (RelTime r) dow timeM = do
    today <- utctDay <$> getCurrentTime
    let dayE = offsetDate r dow today    -- Offset the date w.r.t. the day of week.
    case dayE of
        Right day -> return $ makeAbsDeadline day timeM
        Left err -> throwIO err

-- Creates an absoluted deadline from a day and a potential time.
makeAbsDeadline :: Day -> Maybe TimeOfDay -> Deadline
makeAbsDeadline day (Just time) = Abs (Time $ UTCTime day (timeOfDayToTime time))
makeAbsDeadline day Nothing = Abs (Date day)

-- Offset the date based on the relative time, day of week and today's date.
offsetDate :: RelativeTime -> DayOfWeek -> Day -> Either DateTimeException Day
offsetDate Today _ today = Right today
offsetDate Tomorrow _ today = Right (addDays 1 today)
offsetDate ThisMonth dow today = Left AmbiguousDay      -- "Tuesday this month" is meaningless
offsetDate NextMonth dow today = Left AmbiguousDay
offsetDate w dow today = case w of
    ThisWeek -> if twd < wd     -- Need to check whether day of week is later than today
                then Right (fromMondayStartWeek thisYear tw wd)
                else Left PastDate
    NextWeek -> Right $ fromMondayStartWeek thisYear (tw + 1) wd
    where (tw, twd) = mondayStartWeek today
          wd = fromEnum dow + 1
          (thisYear,_,_) = toGregorian today
