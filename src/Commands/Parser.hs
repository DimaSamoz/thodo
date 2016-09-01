module Commands.Parser
    ( Command (..)
    , parseCommand
    ) where

import Options.Applicative
import Types
import Data.Time hiding (parseTime)
import Commands.Add
import Commands.What
import Commands.Tick
import Commands.Clear

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
parseWhat = What <$> parseWhatCategory

parseTick :: Parser Command
parseTick = Tick <$> parseTickCategory

parseClear :: Parser Command
parseClear = Clear <$> parseClearTarget

parseInit :: Parser Command
parseInit = pure Init


parseCommand :: Parser Command
parseCommand = subparser $
       command "add"   (parseAdd  `withInfo` "Add a new TODO-TASK to the list")
    <> command "what"  (parseWhat `withInfo` "See the tasks from the given categories")
    <> command "tick"  (parseTick `withInfo` "Mark tasks and items as done")
    <> command "clear" (parseClear `withInfo` "Remove tasks from the list")
    <> command "init"  (parseInit `withInfo` "Initialise a new to-do list")



withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
