-- | Subparsers for the 'clear' command.
module Commands.Clear.Parser
    ( parseClearTarget
    ) where

import Types
import Options.Applicative

ticked :: Parser String
ticked = flag' "ticked"
     ( long "ticked"
    <> short 't'
    <> help "Clear ticked tasks")

allTasks :: Parser String
allTasks = flag' "all"
     ( long "all"
    <> short 'A'
    <> help "Clear all tasks")


-- | Parses the category of the task.
parseClearTarget :: Parser String
parseClearTarget = ticked <|> allTasks
