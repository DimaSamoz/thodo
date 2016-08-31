module Commands.Parser
    ( Command (..)
    , parseCommand
    ) where

import Options.Applicative
import Types
import Data.Time hiding (parseTime)
import Commands.Add

data Command
    = Add TodoTask Category Priority AddItems (Maybe DayOfWeek) (Maybe Day) (Maybe TimeOfDay) deriving (Eq, Show)


parseAdd :: Parser Command
parseAdd = Add
    <$> parseTask
    <*> parseCategory
    <*> parsePriority
    <*> parseAddItems
    <*> parseDayOfWeek
    <*> parseDate
    <*> parseTime

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand = subparser $
    command "add"   (parseAdd   `withInfo` "Add a new TODO-TASK to the list")
