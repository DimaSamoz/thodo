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
data Priority = Low | Normal | High deriving (Eq, Show, Ord)

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
    compare (Custom _) (RelTime _) = LT

-- | The absolute deadline of the task.
data AbsoluteTime = Date Day | Time UTCTime deriving (Eq, Show)

-- | The timescale of the deadline, e.g. today//tomorrow or this month//next month.
data Timescale = Days | Weeks | Months | Other deriving (Eq, Show, Ord)

-- | Whether the task or item is done or not.
type Done = Bool

-- | The description of the task or item.
type Description = String

-- | An item in a list task.
data Item = Item
    { _itemDesc :: Description
    , _itemDone :: Done
    } deriving (Eq, Show)


-- | A list task consisting of a description, a list of items (e.g. grocery list), deadline, priority and status.
-- | The list of items can be empty - in that case, the todo task is the description itself.
data Task = Task
    { _desc :: Description
    , _items :: [Item]
    , _deadline :: Deadline
    , _priority :: Priority
    , _done :: Done
    } deriving (Eq, Show)

instance Ord Task where
    compare task1 task2 = _priority task1 `compare` _priority task2


-- | A group of tasks, usually by time or category
data TaskGroup = TaskGroup { _category :: Category, _tasks :: [Task]} deriving (Eq, Show)

instance Ord TaskGroup where
    compare group1 group2 = _category group1 `compare` _category group2


-- | A block of task groups, usually by timescale
data GroupBlock = GroupBlock { _scale :: Timescale, _groups :: [TaskGroup]} deriving (Eq, Show)

instance Ord GroupBlock where
    compare block1 block2 = _scale block1 `compare` _scale block2

-- | An entire to do list with a header containing the name and date, and the list of the blocks in the list.
data TodoList = TodoList { _header :: (String, Day), _blocks :: [GroupBlock]} deriving (Eq, Show)
