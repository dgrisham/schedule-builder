module Command.Utils where

-- Imports
-- =======

import Control.Monad.State


-- Utils
-- =====

class Command a where
    runCommand :: a -> Env -> CommandResult
    --executeCommand :: a -> Env -> HL.InputT IO Env

data CommandResult = UpdatedEnv (Maybe String, Env)
                   | Output String
                   | Failed Error
type Error = String

--type CommandResult = Either Error (CommandOutput, Env)

