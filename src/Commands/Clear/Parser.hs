-- | Subparsers for the 'clear' command.
module Commands.Clear.Parser
    ( parseClearTarget
    ) where

import Types
import Commands.Common
import Options.Applicative

ticked :: Parser ClearTarget
ticked = flag' Ticked
     ( long "ticked"
    <> short 't'
    <> help "Clear ticked tasks")

allTasks :: Parser ClearTarget
allTasks = flag' All
     ( long "all"
    <> short 'A'
    <> help "Clear all tasks")


-- | Parses the category of the task.
parseClearTarget :: Parser ClearTarget
parseClearTarget = ticked <|> allTasks
