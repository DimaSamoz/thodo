-- | Subparsers for the 'add' command.
module Commands.Add.Parser
    ( parseTodoTask
    , parseAddCategory
    , parsePriority
    , parseAddItems
    , parseDayOfWeek
    , parseDate
    , parseTime
    ) where

import Types
import Commands.Common
import Options.Applicative
import Data.Time hiding (parseTime)


-- | Parses a task description.
parseTodoTask :: Parser TodoTask
parseTodoTask = strArgument (metavar "TODO-TASK")

-- | Parses the category of the task.
parseAddCategory :: Parser Category
parseAddCategory =
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
