module Parser.Schedule where

-- Imports
-- =======

import Text.Megaparsec
import Text.Megaparsec.String

-- Local
-- -----

import Types.Schedule

-- Parser
-- ======

p_schedule :: Parser Schedule
p_schedule = Schedule <$> p_taskSlots

p_taskSlots :: Parser [TaskSlot]
p_taskSlots = some p_taskSlot

p_taskSlot :: Parser TaskSlot
p_taskSlot = do
    timeSlot <- p_timeSlot <* skipSome p_space
    task     <- p_task <* newline
    return $ TaskSlot timeSlot task

p_timeSlot :: Parser TimeSlot
p_timeSlot = do
    start <- p_time <* char '-'
    end   <- p_time
    return $ TimeSlot start end

p_task :: Parser Task
p_task =  choice [ try p_task_blank, try p_task_filled, p_task_reserved ]

p_task_blank :: Parser Task
p_task_blank = pipes (some $ char '_') *> return Blank

p_task_filled :: Parser Task
p_task_filled = Filled <$> (string "| " *> some p_taskChar)

p_task_reserved :: Parser Task
p_task_reserved = Reserved <$> some p_taskChar

p_taskChar :: Parser Char
p_taskChar = noneOf ['\n']

p_time :: Parser Time
p_time = concat <$> sequence [ p_digits2, string ":", p_digits2 ]

p_digits2 :: Parser [Char]
p_digits2 = count 2 digitChar

--pipesSpace :: Parser a -> Parser a
--pipesSpace = between (string "| ") (string " |")

pipes :: Parser a -> Parser a
pipes = between (char '|') (char '|')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

p_space :: Parser ()
p_space = satisfy isSpace *> return ()

isSpace :: Char -> Bool
isSpace c = c `elem` [ ' ', '\t' ]

