module Commands.Common
    ( today
    , tomorrow
    , thisWeek
    , nextWeek
    , thisMonth
    , nextMonth
    , customCategory
    , TodoTask
    , AddItems
    , DayOfWeek
    , Command (..)
    ) where

import Types
import Data.Time (Day, TimeOfDay)
import Options.Applicative

data Command
    = Add { addTask :: TodoTask
          , addCat :: Category
          , addPri :: Priority
          , addItems :: AddItems
          , addDOW :: Maybe DayOfWeek
          , addDate :: Maybe Day
          , addTime :: Maybe TimeOfDay
          }
    | What [Category]
    | Tick [Category]
    | Clear String
    | Init deriving (Eq, Show)

-- | The string describing the task.
type TodoTask = String

-- | Whether to add items to the task.
type AddItems = Bool

-- | The days of the week.
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Show, Read)


-- Category parsers

today :: Parser Category
today = flag' (RelTime Today)
     ( long "today"
    <> short 't'
    <> help "Tasks from today")

tomorrow :: Parser Category
tomorrow = flag' (RelTime Tomorrow)
     ( long "tomorrow"
    <> short 'T'
    <> help "Tasks from tomorrow")

thisWeek :: Parser Category
thisWeek = flag' (RelTime ThisWeek)
     ( long "this-week"
    <> short 'w'
    <> help "Tasks from later this week")

nextWeek :: Parser Category
nextWeek = flag' (RelTime NextWeek)
     ( long "next-week"
    <> short 'W'
    <> help "Tasks from next week")

thisMonth :: Parser Category
thisMonth = flag' (RelTime ThisMonth)
     ( long "this-month"
    <> short 'm'
    <> help "Tasks from later this month")

nextMonth :: Parser Category
nextMonth = flag' (RelTime NextMonth)
     ( long "next-month"
    <> short 'M'
    <> help "Tasks from next month")

customCategory :: Parser Category
customCategory = Custom <$> strOption
     ( long "category"
    <> short 'c'
    <> metavar "CATEGORY"
    <> help "Tasks in the category CATEGORY" )
