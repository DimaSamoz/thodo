module Types
    ( Priority (..)
    , Deadline (..)
    , Relative (..)
    , Absolute (..)
    , Status (..)
    , Description
    , Item
    , Task (..)
    ) where
import Data.Time

-- | The priority of the task.
data Priority = Low | Medium | High deriving (Eq, Show)

-- | The deadline of the task.
data Deadline = None | Relative | Absolute deriving (Eq, Show)

-- | The relative deadline of the task.
data Relative = Today | Tomorrow | Days Int | NextWeek | NextMonth deriving (Eq, Show)

-- | The absolute deadline of the task.
data Absolute = Date Day | Time UTCTime deriving (Eq, Show)

-- | The status of the task.
data Status = Todo | Done | Overdue deriving (Eq, Show)

-- | The description of the task.
type Description = String

-- | An item in a list task.
type Item = String

-- | A list task consisting of a description, a list of items (e.g. grocery list), deadline, priority and status.
-- | The list of items can be empty - in that case, the todo task is the description itself.
data Task = Task
    { lDesc :: Description
    , lItems :: [Item]
    , lDeadline :: Deadline
    , lPriority :: Priority
    , lStatus :: Status
    } deriving (Eq, Show)
