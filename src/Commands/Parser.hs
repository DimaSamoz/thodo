module Commands.Parser
    ( Command (..)
    , parseCommand
    ) where

import Options.Applicative
import Types
import Data.Time hiding (parseTime)
import Commands.Add
import Commands.What

data Command
    = Add TodoTask Category Priority AddItems (Maybe DayOfWeek) (Maybe Day) (Maybe TimeOfDay)
    | What [Category] deriving (Eq, Show)


parseAdd :: Parser Command
parseAdd = Add
    <$> parseTask
    <*> parseAddCategory
    <*> parsePriority
    <*> parseAddItems
    <*> parseDayOfWeek
    <*> parseDate
    <*> parseTime

parseWhat :: Parser Command
parseWhat = What
    <$> parseWhatCategory

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand = subparser $
       command "add"   (parseAdd   `withInfo` "Add a new TODO-TASK to the list")
    <> command "what"  (parseWhat `withInfo` "See the tasks from the given categories")
