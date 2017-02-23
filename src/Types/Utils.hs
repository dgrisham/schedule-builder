module Types.Utils
    ( showWithIndices
    ) where
    
-- Imports
-- =======


-- Functionality
-- =============

showWithIndices :: Char -> Char -> ([String] -> [String])
showWithIndices c1 c2 = zipWith (++) indices
    where
        indices = map (showIndex c1 c2) [0..]

-- Helper functions
-- ----------------

showIndex :: Char -> Char -> Int -> String
showIndex c1 c2 n = [c1] ++ show n ++ [c2] ++ spaces
    where
        spaces = replicate k ' '
        k = 5 - quot n 10

