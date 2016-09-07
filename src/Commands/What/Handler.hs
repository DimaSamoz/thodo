-- | Handler for the 'what' command.
module Commands.What.Handler
    ( handleWhatCommand
    ) where

import Types
import Commands.Common
import Options.Applicative

-- | Handler for the 'add' command.
handleWhatCommand :: Command -> IO ()
