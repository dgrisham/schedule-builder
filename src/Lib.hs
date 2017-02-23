module Lib
    ( run
    ) where

-- Imports
-- =======

import System.Environment (getArgs)
import Data.Maybe (Maybe (..))
import Text.Megaparsec (runParser, parseErrorPretty)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate (toWeekDate)
import Text.Printf (printf)

-- Local
-- -----

import Utils (repl)
import Parser.Todo (p_todoList)
import Parser.Schedule (p_schedule)
import Types
import Types.Todo
import Types.Schedule


-- Main
-- ====

run :: IO ()
run = do
    today <- utctDay <$> getCurrentTime
    maybeSchedule <- loadSchedule today
    maybeTodoList <- loadTodoList
    case (maybeSchedule, maybeTodoList) of
        (Just schedule, Just todoList) -> repl (Env schedule todoList) *> return ()
        _                              -> return ()
    return ()

loadSchedule :: Day -> IO (Maybe Schedule)
loadSchedule today = do
    let scheduleFile = getScheduleFile today
    parseResult <- parseFromFile p_schedule scheduleFile
    case parseResult of
        Left error     -> err (parseErrorPretty error)
        Right schedule -> return $ Just schedule

loadTodoList :: IO (Maybe TodoList)
loadTodoList = do
    parseResult <- parseFromFile p_todoList todoFile
    case parseResult of
        Left error     -> err (parseErrorPretty error)
        Right todoList -> return $ Just todoList

-- Helper functions/values
-- -----------------------

getScheduleFile :: Day -> FilePath
getScheduleFile day = schedulePrefix ++ format (toGregorian day)
    where
        format :: (Integer, Int, Int) -> FilePath
        format (year, month, day) =
            printf "%d-%02d-%02d.md" year month day

schedulePrefix :: FilePath
schedulePrefix = "/home/grish/personal/schedule/daily/"

todoFile :: FilePath
todoFile = "/home/grish/personal/schedule/todo.md"

err :: String -> IO (Maybe a)
err errorStr = putStr errorStr >> return Nothing

parseFromFile p file = runParser p file <$> readFile file

