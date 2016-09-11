{-# LANGUAGE TemplateHaskell, Rank2Types #-}

-- | Functions utilising lenses into the to-do lists.
module TodoLenses
    ( addTask
    , getGroupByCategory
    , tickTask
    , tickItem
    ) where

import Types
import Util
import Data.Time
import Control.Lens

makeLenses ''Item
makeLenses ''Task
makeLenses ''TaskGroup
makeLenses ''GroupBlock
makeLenses ''TodoList

-- | Inserts a task into the correct block and group of the to-do list.
addTask :: TodoList -> Task -> Category -> IO TodoList
addTask list task cat =
    return $ list
        & blocks
        . findBy scale (categoryToTimescale cat)
        . groups
        . findBy category cat
        . tasks %~ (task :)

-- | Returns a task group given its category.
getGroupByCategory :: TodoList -> Category -> TaskGroup
getGroupByCategory list cat =
    list^.blocks.findBy scale (categoryToTimescale cat).groups.findBy category cat

tickTask :: TodoList -> Category -> Int -> TodoList
tickTask list cat taskIx =
    list
        & blocks
        . findBy scale (categoryToTimescale cat)
        . groups
        . findBy category cat
        . tasks
        . element taskIx
        . done .~ True

tickItem :: TodoList -> Category -> Int -> Int -> TodoList
tickItem list cat taskIx itemIx =
    list
        & blocks
        . findBy scale (categoryToTimescale cat)
        . groups
        . findBy category cat
        . tasks
        . element taskIx
        . items
        . element itemIx
        . itemDone .~ True


-- | Lens that finds an element in a list that matches another based on the specified label.
findBy :: Eq a => Getting a s a -> a -> Lens' [s] s
findBy label ts = lens get set
    where get = customHead . filter (\b -> b^.label == ts)
          set = replaceInList (\b -> b^.label == ts)
          -- Head function with a custom error message - a bit smelly but errors are handled by optparse-applicative
          -- so this is not much different from an exception.
          customHead [] = error "Category is empty or doesn't exist."
          customHead l = head l

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
