module Utils where


-- Imports
-- =======

import Prelude hiding (showList)
import Data.List.Split (splitOn)
import qualified System.Console.Haskeline as HL

-- Local
-- -----

import Types
import Types.Todo
import Types.Schedule


-- Utils
-- =====

repl :: Env -> IO Env
repl env =
    HL.runInputT HL.defaultSettings (repl' env)
    where
        repl' :: Env -> HL.InputT IO Env
        repl' env = do
            minput <- HL.getInputLine "schedule-builder $ "
            case minput of
                Nothing     -> repl' env
                Just "exit" -> return env
                Just input  -> HL.outputStr "\n"
                    *> processInput (splitOn " " input) env
                    *> repl' env

-- Input handler
-- -------------

processInput :: [String] -> Env -> HL.InputT IO Env

processInput ("help":args) env = (HL.outputStrLn $ getHelp args) *> return env

processInput ("print":args) env =
    runPrint args env *> return env

processInput ("update":args) env =
    output (envSchedule env') *> return env'
    where
        env' = updateSchedule args env

-- shorter aliases
processInput ("h":args) env = processInput ("help":args) env
processInput ("p":args) env = processInput ("print":args) env
processInput ("u":args) env = processInput ("update":args) env
processInput _ env = invalidInput *> return env

updateSchedule :: [String] -> Env -> Env
updateSchedule (category:todoNum:slot:[]) env =
    case lookupTodoLabel todoList category todoNum' of
        Nothing    -> env
        Just label ->
            env { envSchedule = scheduleTask slotNum schedule (Filled label) }
    where
        slotNum  = read slot    :: Int
        todoNum' = read todoNum :: Int
        schedule = envSchedule env
        todoList = envTodoList env

-- ### Help message

getHelp :: [String] -> String
getHelp _ = "Implemented commands:\n"
         ++ (showStrList commands)

commands :: [String]
commands = [ "help"
           , "print"
           , "update"
           ]

-- #### Helper functions

showStrList :: [String] -> String
showStrList = unlines . map ((++) "-   ")

-- ### Print commands

runPrint :: [String] -> Env -> HL.InputT IO ()



-- Helper functions
-- ----------------

--outputStr = HL.outputStrLn . (++ "\n")

-- ### Colors

--colors :: String -> String
