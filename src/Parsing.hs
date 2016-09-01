-- | Functions to parse to-do list components from strings.
module Parsing
    ( parseItem
    , parseTask
    , parseGroup
    , parseBlock
    , parseTodoList
    ) where

import           Control.Applicative hiding (many, optional, (<|>))
import           Control.Monad       (void)
import           Data.List           (isInfixOf)
import           Data.Time           (Day, UTCTime, defaultTimeLocale,
                                      parseTimeOrError)
import           Text.Parsec
import           Text.Show.Pretty
import           Types

parse' :: Parsec String () c -> String -> Either ParseError c
parse' rule = parse rule "Parsing.hs"

-- | Parses an item.
parseItem :: Parsec String () Item
parseItem = do
    count 6 space                   -- Leading tab spaces
    tickOrSpace <- anyChar          -- Whether the item is done or not
    count 4 anyChar                 -- Spaces and index
    desc <- manyTill anyChar newline    -- Description of item
    return Item {_itemDesc = desc, _itemDone = tickOrSpace /= ' '}

-- Parses a date separator.
dateSep :: Parsec String () Char
dateSep = char '-' >> space

-- Parses an absolute deadline, if it exists.
parseAbsDeadline :: String -> Deadline
parseAbsDeadline "" = None
parseAbsDeadline str            -- Optimistically allow for errors in the parsing – hopefully none will occur.
    | ',' `elem` str = Abs (Time $ parseTimeOrError True defaultTimeLocale "%R, %A %e %b %G" str)
    | otherwise      = Abs (Date $ parseTimeOrError True defaultTimeLocale "%A %e %b %G" str)

-- Parses the task and its date.
parseTaskDescDate :: Parsec String () Deadline
parseTaskDescDate = do
    dateSep <|> lookAhead newline           -- Either parse the date separator or the newline, not consuming the latter
    remainder <- anyChar `manyTill` newline -- Parse the rest into a string
    return $ parseAbsDeadline remainder     -- Parse and return the deadline.

-- Parses the priority token that describes the priority of the task.
priorityToken :: Parsec String () Priority
priorityToken = do
    -- The trailing spaces for undated tasks might be removed, so we need to account for that possibility
    token <- try (string "!  ")     <|> string "!"
         <|> try (string "...  ")   <|> string "..."
         <|> try (string "  ")      <|> lookAhead (string "\n") -- We need to match the empty string at the end of the line, without consuming the newline
    case head token of
        '!' -> return High
        '\n' -> return Normal
        ' '  -> return Normal
        '.'  -> return Low

-- Parses the whole task description.
parseTaskDesc :: Parsec String () (Description, Priority, Deadline)
parseTaskDesc =
    -- Use the applicative style, since the individual components are independent
    (,,) <$> anyChar `manyTill` lookAhead priorityToken -- We don't want to consume the token, only look ahead
         <*> priorityToken
         <*> parseTaskDescDate

-- Parses the entire task header.
parseTaskHeader :: Parsec String () Task
parseTaskHeader = do
    count 2 space
    tickOrSpace <- anyChar      -- Ignore tabs and index, save check or space
    count 5 anyChar
    (desc, priority, deadline) <- parseTaskDesc
    return Task
        { _desc = desc
        , _items = []
        , _deadline = deadline
        , _priority = priority
        , _done = tickOrSpace /= ' '
        }

-- | Parses a task.
parseTask :: Deadline -> Parsec String () Task
parseTask relTime = do
    header <- parseTaskHeader
    taskItems <- many $ try parseItem
    if _deadline header == None      -- If the task doesn't have an absolute deadline, set the relative deadline from the group
        then return header {_items = taskItems, _deadline = relTime}
        else return header {_items = taskItems}

tryNoTasks :: Parsec String () Bool
tryNoTasks = do
    notasks <- try (string "    Nothing yet.\n") <|> string ""
    return $ notasks /= ""

-- Parses the group category.
parseCategory :: Parsec String () Category
parseCategory = do
    category <- anyChar `manyTill` newline
    return $ case category of
        "TODAY"         -> RelTime Today
        "TOMORROW"      -> RelTime Tomorrow
        "THIS WEEK"     -> RelTime ThisWeek
        "NEXT WEEK"     -> RelTime NextWeek
        "THIS MONTH"    -> RelTime ThisMonth
        "NEXT MONTH"    -> RelTime NextMonth
        "OVERDUE"       -> RelTime Overdue
        other           -> Custom other

-- | Parses a task group.
parseGroup :: Parsec String () TaskGroup
parseGroup = do
    category <- parseCategory
    notasks <- tryNoTasks
    tasks <- if notasks
            then return []
            else case category of
                RelTime rt  -> many $ try (parseTask (Rel rt))
                Custom _    -> many $ try (parseTask None)
    return TaskGroup {_category = category, _tasks = tasks}

blockSep :: Parsec String () ()
blockSep = string "____" >> newline >> void newline

-- Converts a group category into a timescale.
determineTimescale :: TaskGroup -> Timescale
determineTimescale group = case _category group of
    RelTime Today -> Days
    RelTime Tomorrow -> Days
    RelTime ThisWeek -> Weeks
    RelTime NextWeek -> Weeks
    RelTime ThisMonth -> Months
    RelTime NextMonth -> Months
    Custom _ -> Other

-- | Parses a group block.
parseBlock :: Parsec String () GroupBlock
parseBlock = do
    groups <- parseGroup `sepBy` newline                -- Groups are separated by newlines
    let timescale = determineTimescale (head groups)    -- Timescale is determined from the category of the first group
    return GroupBlock { _scale = timescale, _groups = groups }

-- Parses the to-do list header
parseListHeader :: Parsec String () (String, Day)
parseListHeader = do
    newline >> spaces                          -- Ignores the newline and spaces
    name <- anyChar `manyTill` char '\''       -- Reads the user's name
    anyChar `manyTill` newline >> spaces       -- Ignores the rest of the line and the spaces
    dateString <- anyChar `manyTill` newline
    let dateP = parseTimeOrError True defaultTimeLocale "%e %b %G" dateString :: Day
    newline
    return (name, dateP)

-- | Parses the to-do list.
parseTodoList :: Parsec String () TodoList
parseTodoList = do
    header <- parseListHeader
    blocks <- parseBlock `sepBy` blockSep
    return TodoList {_header = header, _blocks = blocks}


-- Examples - to be removed
task :: String
task = "\n    Dima's To Do List\n       17 Sep 2016\n\n"
    ++ "TODAY\n  ✔ 1.  Grocery shopping  \n        a) Bread\n      ✔ b) Milk\n        c) Toothpaste\n    2.  Do stuff...  \n\n"
    ++ "TOMORROW\n  ✔ 1.  Call Simon!  \n  ✔ 2.  Blabla  \n        a) Harambe\n      ✔ b) Creamy memes\n\n"
    ++ "OVERDUE\n    1.  Update OS...  - Friday 16 Sep 2016\n"
    ++ "____\n\n"
    ++ "THIS WEEK\n    1.  Fix bike!  \n    2.  Call Boris...  \n\n"
    ++ "NEXT WEEK\n  ✔ 1.  Email Rob!  \n  ✔ 2.  Blabla  \n        a) Harambe\n      ✔ b) Creamy memes\n"
    ++ "____\n\n"
    ++ "BIRTHDAYS\n    1.  Donald  - Thursday 15 Sep 2016  \n    2.  Gerthrude  - Saturday 17 Sep 2016  \n\n"
    ++ "WISHLIST\n    1.  Movies to watch  \n        a) Deadpool\n      ✔ b) Suicide Squad\n"


t = parse' parseTodoList task

nice a = putStrLn $ ppShow a
