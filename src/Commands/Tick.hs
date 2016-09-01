-- | Subparsers for the 'tick' command.
module Commands.Tick
    ( parseTickCategory
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
parseTickCategory :: Parser [Category]
parseTickCategory =
    some  ( today
        <|> tomorrow
        <|> thisWeek
        <|> nextWeek
        <|> thisMonth
        <|> nextMonth
        <|> customCategory
        <|> still )
    <|> pure [RelTime Today, RelTime Tomorrow]
