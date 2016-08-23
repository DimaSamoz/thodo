-- | The data types used in the to do list application.
module Types
    ( Priority (..)
    , Deadline (..)
    , RelativeTime (..)
    , AbsoluteTime (..)
    , Timescale (..)
    , Category (..)
    , Done
    , Description
    , Item (..)
    , Task (..)
    , TaskGroup (..)
    , GroupBlock (..)
    , TodoList (..)
    ) where
import Data.Time

-- | The priority of the task.
data Priority = Low | Medium | High deriving (Eq, Show, Ord)

-- | The deadline of the task.
data Deadline = None | Rel RelativeTime | Abs AbsoluteTime deriving (Eq, Show)

-- | The relative deadline of the task.
data RelativeTime
    = Today
    | Tomorrow
    | ThisWeek
    | NextWeek
    | ThisMonth
    | NextMonth
    | Overdue
    deriving (Eq, Ord)

instance Show RelativeTime where
    show Today = "Today"
    show Tomorrow = "Tomorrow"
    show ThisWeek = "This week"
    show NextWeek = "Next week"
    show ThisMonth = "This month"
    show NextMonth = "Next month"
    show Overdue = "Overdue"

-- | The task category a task group belongs to.
data Category = RelTime RelativeTime | Custom String deriving (Eq)

instance Show Category where
    show (RelTime t) = show t
    show (Custom str) = str

instance Ord Category where
    compare (RelTime t1) (RelTime t2) = compare t1 t2
    compare (Custom st1) (Custom st2) = compare st1 st2
    compare (RelTime _) (Custom _) = GT

-- | The absolute deadline of the task.
data AbsoluteTime = Date Day | Time UTCTime deriving (Eq, Show)

-- | The timescale of the deadline, e.g. today//tomorrow or this month//next month.
data Timescale = Days | Weeks | Months | Other String deriving (Eq, Show, Ord)

-- | Whether the task or item is done or not.
type Done = Bool

-- | The description of the task or item.
type Description = String

-- | An item in a list task.
data Item = Item
    { itemDesc :: Description
    , itemDone :: Done
    } deriving (Eq, Show)


-- | A list task consisting of a description, a list of items (e.g. grocery list), deadline, priority and status.
-- | The list of items can be empty - in that case, the todo task is the description itself.
data Task = Task
    { desc :: Description
    , items :: [Item]
    , deadline :: Deadline
    , priority :: Priority
    , done :: Done
    } deriving (Eq, Show)

instance Ord Task where
    compare task1 task2 = priority task1 `compare` priority task2


-- | A group of tasks, usually by time or category
data TaskGroup = TaskGroup { category :: Category, tasks :: [Task]} deriving (Eq, Show)

instance Ord TaskGroup where
    compare group1 group2 = category group1 `compare` category group2


-- | A block of task groups, usually by timescale
data GroupBlock = GroupBlock { scale :: Timescale, groups :: [TaskGroup]} deriving (Eq, Show)

instance Ord GroupBlock where
    compare block1 block2 = scale block1 `compare` scale block2

-- | An entire to do list with a header containing the name and date, and the list of the blocks in the list.
data TodoList = TodoList { header :: (String, Day), blocks :: [GroupBlock]}
