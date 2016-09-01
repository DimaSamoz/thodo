-- | Functions to print list components and convert them to strings.
module Printing
    ( showItem
    , showTask
    , showGroup
    , showBlock
    , showTodoList
    , printStrings
    , writeToFile
    ) where

import Types
import Data.Time
import Data.List (sort, sortBy, intersperse, intercalate)
import Data.Char (toUpper)


spaces :: Int -> String
spaces num = replicate num ' '

tick :: String
tick = "  ✔ "

separator :: String
separator = "____"

noTasks :: String
noTasks = "    Nothing yet."

padIndex :: Int -> String
padIndex index
    | index < 10  = show index ++ ".  "
    | otherwise   = show index ++ ". "

showItem :: (Char, Item) -> String
showItem (index, item)
    | itemDone item = concat [spaces 4, tick, index : ") ", itemDesc item]
    | otherwise     = concat [spaces 8, index : ") ", itemDesc item]

showPriority :: Priority -> String
showPriority Low    = "...  "
showPriority Normal = "  "
showPriority High   = "!  "

showAbsDeadline :: Deadline -> String
showAbsDeadline (Abs (Date d)) = " – " ++ formatTime defaultTimeLocale "%A %e %b %G" d
showAbsDeadline (Abs (Time t)) = " – " ++ formatTime defaultTimeLocale "%R, %A %e %b %G" t
showAbsDeadline _ = ""

showTaskHeader :: Int -> Task -> String
showTaskHeader index task
    | done task = concat [tick, padIndex index, text]
    | otherwise = concat [spaces 4, padIndex index, text]
        where text = concat [desc task, showPriority (priority task), showAbsDeadline (deadline task)]

-- | Converts a task into a string list.
showTask :: (Int, Task) -> [String]
showTask (index, task) = showTaskHeader index task : map showItem indexedItems
    where indexedItems = zip ['a'..] (items task)

showGroupHeader :: TaskGroup -> String
showGroupHeader (TaskGroup (Custom str) _) = map toUpper str
showGroupHeader (TaskGroup time _) = map toUpper $ show time

-- | Converts a task group into a string list.
showGroup :: TaskGroup -> [String]
showGroup group
    | null (tasks group) = showGroupHeader group : [noTasks]
    | otherwise          = showGroupHeader group : concatMap showTask indexedTasks
        where indexedTasks = zip [1..] (sortBy (flip compare) $ tasks group)

-- | Converts a group block into a string list.
showBlock :: GroupBlock -> [String]
showBlock block = intercalate [""] $ map showGroup sortedGroups
    where sortedGroups = sort $ groups block

showListHeader :: TodoList -> [String]
showListHeader (TodoList (name, date) _) = ["", spaces 4 ++ title, spaces (4 + centOffset) ++ dateString,""]
    where title = name ++ "'s To Do List"
          dateString = formatTime defaultTimeLocale "%e %b %G" date
          centOffset = (length title - length dateString) `div` 2

-- | Converts a to do list into a string list.
showTodoList :: TodoList -> [String]
showTodoList list = showListHeader list ++ intercalate [separator ++ "\n"] (map showBlock sortedBlocks)
    where sortedBlocks = sort $ blocks list

exItems :: [Item]
exItems =
    [ Item "Bread" False
    , Item "Milk" True
    , Item "Toothpaste" False
    ]

exTask :: Task
exTask = Task "Grocery shopping" exItems (Rel Today) Normal True

exGroup :: TaskGroup
exGroup = TaskGroup (RelTime Today)
    [ Task "Do laundry" [] (Rel Today) Low False
    , Task "Call Simon" [] (Rel Today) High False
    , exTask
    , Task "Print tickets" [] (Abs $ Date (fromGregorian 2016 9 24)) High False
    , Task "Do important stuff" [] (Abs $ Time (UTCTime (fromGregorian 2005 3 5) (timeOfDayToTime $ dayFractionToTimeOfDay 0.76))) High False

    ]

exBlock :: GroupBlock
exBlock = GroupBlock Days
    [ exGroup
    , TaskGroup (RelTime Tomorrow)
        [ Task "Hi" [] (Rel Tomorrow) High True
        , Task "Email" [] (Rel Tomorrow) Low False
        ]
    ]

exBlock2 :: GroupBlock
exBlock2 = GroupBlock Weeks
    [ TaskGroup (RelTime ThisWeek) []
    , TaskGroup (RelTime NextWeek)
        [ Task "Eat"
            [ Item "Blabla" True
            , Item "Wibble" False
            ] (Rel NextWeek) Normal False
        ]
    ]

exList :: TodoList
exList = TodoList ("Dima", fromGregorian 2016 8 23)
    [ exBlock
    , exBlock2
    ]

-- | Print a todo list in the console.
printStrings :: [String] -> IO ()
printStrings = mapM_ putStrLn

-- | Write the to-do list into a file.
writeToFile :: FilePath -> TodoList -> IO ()
writeToFile fileName = writeFile fileName . unlines . showTodoList
