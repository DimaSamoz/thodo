module Commands.Parser
    ( parseCommand
    , handleCommand
    ) where

import Options.Applicative
import Types
import Data.Time hiding (parseTime)
import Commands.Add
import Commands.What
import Commands.Tick
import Commands.Clear
import Commands.Init
import Commands.Common


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


handleCommand :: Command -> IO ()
handleCommand Init = handleInitCommand


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
