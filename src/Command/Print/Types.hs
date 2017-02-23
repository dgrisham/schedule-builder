module Command.Print.Types

-- Imports
-- =======

-- Local
-- -----

import Types.Command.Utils


-- Types
-- =====

data Print = PrintSchedule
           | PrintTodo (Maybe Query)
           | PrintNotes Category TodoNum
           --| PrintHelp

type Query = String
type TodoNum = Int

instance Command Print where
    runCommand :: Print -> Env -> CommandResult

    -- print the current schedule
    runCommand (PrintSchedule) env =
        Output output
        where
            output = return . show $ schedule
            schedule =  envSchedule env

    -- print the todo list
    runCommand (PrintTodo (Nothing)) env =
        Output output
        where
            output = return . show $ todoList
            todoList = envTodoList env

    -- print subset of todo list
    runCommand (PrintTodo (Just query)) env =
        Output output
        where
            output = return . show $ filteredTodoList
            filteredTodoList = fuzzyFilter todoList query
            todoList = envTodoList env

    --  print notes for a particular task
    runCommand (PrintNotes category todoNum) env =
        case showTodoNotes (envTodoList env) category todoNum of
            Just notes -> Output output
            Nothing    -> Failed
        where
            output = return . show $ notes

