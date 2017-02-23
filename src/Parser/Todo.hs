module Parser.Todo where

-- Imports
-- =======

import Text.Megaparsec
import Text.Megaparsec.String

-- Local
-- -----

import Types.Todo


-- Parser
-- ======

p_todoList :: Parser TodoList
p_todoList = TodoList <$> many p_todoGroup

p_todoGroup :: Parser TodoGroup
p_todoGroup = do
    category <- p_category
    todos    <- p_todos <* string "\n\n"
    return $ TodoGroup category todos

p_category :: Parser Category
p_category = p_header '='

p_todos :: Parser [Todo]
p_todos = many p_todo

p_todo :: Parser Todo
p_todo = Todo <$> p_todoLabel <*> p_notes

p_todoLabel :: Parser Label
p_todoLabel = p_listItem

p_notes :: Parser Notes
p_notes = many $ p_note

p_note :: Parser Note
p_note = do
    level <- length <$> (p_tab *> many p_tab)
    note  <- p_listItem
    return $ Note level note

-- Helper functions
-- ----------------

p_header :: Char -> Parser String
p_header underline = manyTill anyChar newline
                  <* some (char underline)
                  <* count 2 newline

p_listItem :: Parser String
p_listItem = string "-   " *> manyTill anyChar newline

p_tab :: Parser String
p_tab = count 4 p_space

p_space :: Parser Char
p_space = char ' '

