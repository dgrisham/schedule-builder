module Types.Todo
    ( TodoList (..)
    , TodoGroup (..)
    , Category
    , Todo (..)
    , Label
    , Notes
    , Note (..)
    , Level
    , showTodoNotes
    , showCategories
    , lookupTodoLabel
    , lookupTodo
    , fuzzyFilter
    , lookupTodoGroup
    ) where

-- Imports
-- =======

import Data.Char (toUpper, toLower)
import Data.List (find, isSubsequenceOf)

-- Local
-- -----

import Types.Utils (showWithIndices)


-- Types
-- =====

data TodoList = TodoList [TodoGroup]
data TodoGroup = TodoGroup Category [Todo]
type Category = String
data Todo = Todo { todoLabel :: Label
                 , todoNotes :: Notes
                 }
type Label = String
type Notes = [Note]
data Note = Note Level String
type Level = Int

-- Instance declarations
-- ---------------------

instance Show TodoList where
    show (TodoList todoGroups) =
        unlines . map show $ todoGroups

instance Show TodoGroup where
    show (TodoGroup category todos) =
        showHeader category '='
     ++ (unlines . showWithIndices '(' ')' . map show $ todos)

instance Show Todo where
    show (Todo label _) = label

instance Show Note where
    show (Note level note) =
        (concat . replicate level $ tab)
     ++ "-   "
     ++ note

-- ### Helper functions

showHeader :: String -> Char -> String
showHeader header underline =
    header
 ++ "\n"
 ++ (take (length header) $ repeat underline)
 ++ "\n\n"

-- Functionality
-- =============

showTodoNotes :: TodoList -> Category -> Int -> Maybe String
showTodoNotes todoList category i = showTodoNotes' <$> todo
    where
        todo = lookupTodo todoList category i

showCategories :: TodoList -> String
showCategories todoList =
    showHeader "Categories" '='
 ++ (unlines . getCategories $ todoList)

lookupTodoLabel :: TodoList -> Category -> Int -> Maybe Label
lookupTodoLabel t c = (todoLabel <$>) . lookupTodo t c

lookupTodo :: TodoList -> Category -> Int -> Maybe Todo
lookupTodo todoList category n = todoGroup >>= lookupTodo' n
    where
        todoGroup = lookupTodoGroup todoList category

fuzzyFilter :: TodoList -> String -> TodoList
fuzzyFilter (TodoList todoGroups) query =
    TodoList $ filter (isCategorySubsequence query) todoGroups

lookupTodoGroup :: TodoList -> Category -> Maybe TodoGroup
lookupTodoGroup (TodoList todoGroups) category =
    find (isCategorySubsequence category') todoGroups
    where
        category' = toLower' category

-- Helper functions
-- ----------------

showTodoNotes' :: Todo -> String
showTodoNotes' (Todo label notes) =
    showHeader label '-'
 ++ (unlines . map show $ notes)

getCategories :: TodoList -> [Category]
getCategories (TodoList todoGroups) = map getCategory todoGroups

getCategory :: TodoGroup -> Category
getCategory (TodoGroup category _) = category

lookupTodo' :: Int -> TodoGroup -> Maybe Todo
lookupTodo' n (TodoGroup category todos)
    | n < length todos = Just $ todos !! n
    | otherwise = Nothing

hasCategory :: Category -> TodoGroup -> Bool
hasCategory category (TodoGroup groupCategory _) =
    category == (toLower' groupCategory)

isCategorySubsequence :: Category -> TodoGroup -> Bool
isCategorySubsequence category (TodoGroup groupCategory _) =
    isSubsequenceOf category (toLower' groupCategory)

toLower' :: String -> String
toLower' = map toLower

tab :: String
tab = replicate 4 ' '

