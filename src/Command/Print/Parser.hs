module Command.Print.Parser where

-- Imports
-- =======

import Text.Megaparsec
import Text.Megaparsec.String

-- Local
-- -----

import Types.Command.Print


-- Parsers
-- =======

p_print :: Parser Print
p_print = string "print " *> p_print'

p_print' :: Parser Print
p_print' =  p_printSchedule
        <|> p_printTodo
        <|> p_printNotes
        --<|> p_printHelp

p_printSchedule :: Parser Print
p_printSchedule = string "schedule" *> return PrintSchedule

p_printTodo :: Parser Print
p_printTodo = string "todo" *> p_printTodo'

p_printTodo' :: Parser Print
p_printTodo' = PrintTodo
           <$> p_maybe (p_query)

p_query :: Parser Query
p_query = -- TODO

-- Helper parsers
-- --------------

p_maybe :: Parser a -> Parser a
p_maybe p = (Just <$> p) <|> return Nothing

