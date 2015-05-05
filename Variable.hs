module Variable where
data Variable = Var {coefficient :: Float, degree :: Integer} deriving (Show, Eq, Read)
-- The variable is a constant if the degree = 0
