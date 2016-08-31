-- | Subparsers for the 'add' command.
module Commands.Add
    ( parseTask
    , parseCategory
    , parsePriority
    , parseAddItems
    , parseDayOfWeek
    , parseDate
    , parseTime
    , TodoTask
    , AddItems
    , DayOfWeek
    ) where

import Options.Applicative
import Types
import Data.Time hiding (parseTime)

-- | The string describing the task.
type TodoTask = String

-- | Whether to add items to the task.
type AddItems = Bool

-- | The days of the week.
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Show, Read)

-- | Parses a task description.
parseTask :: Parser TodoTask
parseTask = strArgument (metavar "TODO-TASK")

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

-- | Parses the category of the task.
parseCategory :: Parser Category
parseCategory =
        today
    <|> tomorrow
    <|> thisWeek
    <|> nextWeek
    <|> thisMonth
    <|> nextMonth
    <|> customCategory
    <|> pure (RelTime Today)

-- | Parses the add items flag.
parseAddItems :: Parser AddItems
parseAddItems = switch
     ( long "items"
    <> short 'i'
    <> help "Add items to the task" )

-- Priority parsers

high :: Parser Priority
high = flag' High
     ( long "high"
    <> short 'H'
    <> help "High priority")

normal :: Parser Priority
normal = flag' Normal
     ( long "normal"
    <> short 'n'
    <> help "Normal priority")

low :: Parser Priority
low = flag' Low
     ( long "low"
    <> short 'l'
    <> help "Low priority")

-- | Parses the priority of the task.
parsePriority :: Parser Priority
parsePriority =
        high
    <|> normal
    <|> low
    <|> pure Normal

-- | Parses the day of the week.
parseDayOfWeek :: Parser (Maybe DayOfWeek)
parseDayOfWeek = optional $ option auto
     ( long "on"
    <> short 'o'
    <> metavar "DAY-OF-WEEK"
    <> help "The three-letter abbreviation of the day of the week" )

-- | Parses the date.
parseDate :: Parser (Maybe Day)
parseDate = optional $ stringToDate <$> strOption
     ( long "date"
    <> short 'd'
    <> metavar "DATE"
    <> help "The date in dd/MM/yyyy format" )

-- | Parses the time.
parseTime :: Parser (Maybe TimeOfDay)
parseTime = optional $ stringToTime <$> strOption
     ( long "at"
    <> short 'a'
    <> metavar "TIME"
    <> help "The time in hh:mm format" )

-- Modified time locale: dd/mm/yyyy format.
modTimeLocale :: TimeLocale
modTimeLocale = defaultTimeLocale {dateFmt = "%-d/%-m/%Y", timeFmt = "%R"}

-- Parses a date string.
stringToDate :: String -> Day
stringToDate = parseTimeOrError True modTimeLocale "%x"

-- Parses a time string.
stringToTime :: String -> TimeOfDay
stringToTime = parseTimeOrError True modTimeLocale "%X"
