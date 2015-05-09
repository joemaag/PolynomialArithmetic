-- | Provides the Variable data type.

module Variable where

data Variable = Var {coefficient :: Float, degree :: Integer} deriving (Show, Eq, Read)
