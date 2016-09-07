-- | Subparsers for the 'what' command.
module Commands.What.Parser
    ( parseWhatCategory
    ) where

import Types
import Commands.Common
import Options.Applicative

still :: Parser Category
still = flag' (RelTime Overdue)
     ( long "still"
    <> short 's'
    <> help "Overdue tasks")

-- | Parses the category of the task.
parseWhatCategory :: Parser [Category]
parseWhatCategory =
    some  ( today
        <|> tomorrow
        <|> thisWeek
        <|> nextWeek
        <|> thisMonth
        <|> nextMonth
        <|> customCategory
        <|> still )
    <|> pure [RelTime Today, RelTime Tomorrow]
