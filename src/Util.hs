-- | Module containing utility functions, conversion, etc.
module Util
    ( initTodoList
    , prompt
    , categoryToTimescale
    , deadlineToCategory
    , replaceInList
    ) where

import Types
import Data.Time
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import System.Console.Haskeline

-- | Creates a new, empty todo list with the given name and date.
initTodoList :: String -> Day -> TodoList
initTodoList name date =
    TodoList (name, date)
        [ GroupBlock Days
            [ TaskGroup (RelTime Today) []
            , TaskGroup (RelTime Tomorrow) []
            ]
        , GroupBlock Weeks
            [ TaskGroup (RelTime ThisWeek) []
            , TaskGroup (RelTime NextWeek) []
            ]
        , GroupBlock Months
            [ TaskGroup (RelTime ThisMonth) []
            , TaskGroup (RelTime NextMonth) []
            ]
        , GroupBlock Other []
        ]

-- | Prints a prompt message and returns the user's response.
prompt :: String -> IO String
prompt message = runInputT defaultSettings loop
    where
    loop :: InputT IO String
    loop = do
        line    <- getInputLine message
        case line of
            Just str -> return str
            Nothing -> return ""

-- | Converts a group category into a timescale.
categoryToTimescale :: Category -> Timescale
categoryToTimescale (RelTime Today) = Days
categoryToTimescale (RelTime Tomorrow) = Days
categoryToTimescale (RelTime ThisWeek) = Weeks
categoryToTimescale (RelTime NextWeek) = Weeks
categoryToTimescale (RelTime ThisMonth) = Months
categoryToTimescale (RelTime NextMonth) = Months
categoryToTimescale (Custom _) = Other

-- | Converts a deadline to a category.
deadlineToCategory :: Deadline -> IO Category
deadlineToCategory None = return $ RelTime Today
deadlineToCategory (Rel reltime) = return $ RelTime reltime
deadlineToCategory (Abs (Date date)) = do
    currentTime <- getCurrentTime
    return $ dateToCategory (utctDay currentTime) date
deadlineToCategory (Abs (Time time)) = do
    currentTime <- getCurrentTime
    return $ timeToCategory currentTime time

-- Converts a date to a category.
dateToCategory :: Day -> Day -> Category
dateToCategory currentD otherD
    | otherD < currentD = RelTime Overdue   -- Current date after other - task overdue
    | cm + 1 < om = Custom "Future"         -- Date later than next month
    | cm < om = RelTime NextMonth           -- Date next month
    | cwn + 1 < own = RelTime ThisMonth     -- Date this month, but not next week
    | cwn < own = RelTime NextWeek          -- Date next week
    | cd + 1 < od = RelTime ThisWeek        -- Date this week, but not tomorrow
    | cd < od = RelTime Tomorrow            -- Date tomorrow
    | otherwise = RelTime Today             -- Date today
    where c@(_, cm, cd) = toGregorian currentD
          o@(_, om, od) = toGregorian otherD
          (cwn, cwd) = mondayStartWeek currentD
          (own, owd) = mondayStartWeek otherD

-- Converts a time into a category.
timeToCategory :: UTCTime -> UTCTime -> Category
timeToCategory currentT otherT =
    if otherT < currentT
        then RelTime Overdue    -- Need to take into account the time of day, so compare this first
        else dateToCategory (utctDay currentT) (utctDay otherT)

-- | Finds the first element matching a predicate in a list with another element.
replaceInList :: (a -> Bool) -> [a] -> a -> [a]
replaceInList _ [] _ = []
replaceInList p (x:xs) e  = if p x then e:xs else x : replaceInList p xs e
