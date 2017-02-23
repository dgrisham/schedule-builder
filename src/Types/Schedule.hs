module Types.Schedule
    ( Schedule (..)
    , TaskSlot (..)
    , TimeSlot (..)
    , Time (..)
    , Task (..)
    , scheduleTask
    ) where
    
-- Imports
-- =======

-- Local
-- -----

import Types.Utils (showWithIndices)


-- Types
-- =====

data Schedule = Schedule [TaskSlot]

data TaskSlot = TaskSlot { timeSlot :: TimeSlot
                         , task     :: Task
                         }

data TimeSlot  = TimeSlot Time Time
type Time = String

data Task = Reserved String
          | Filled String
          | Blank

-- Instance declarations
-- ---------------------

instance Show Schedule where
    show (Schedule taskSlots) =
        unlines . showWithIndices '[' ']' . map show $ taskSlots

instance Show TaskSlot where
    show (TaskSlot timeSlot task) =
        (show timeSlot) ++ "     " ++ (show task) 

instance Show TimeSlot where
    show (TimeSlot start end) =
        start ++ "-" ++ end

instance Show Task where
    show (Reserved task) =
        task
    show (Filled task) =
        "| " ++ task
    show Blank =
        "|_____________________|"

-- Functionality
-- =============

scheduleTask :: Int -> Schedule -> Task -> Schedule
scheduleTask i (Schedule taskSlots) newTask = Schedule taskSlots'
    where
        taskSlots' = setTask i taskSlots newTask

-- Helper functions
-- ----------------

setTask :: Int -> [TaskSlot] -> Task -> [TaskSlot]
setTask i taskSlots newTask = prefix ++ [oldTask { task = newTask }] ++ suffix
    where
        (prefix, (oldTask:suffix)) = splitAt i taskSlots

