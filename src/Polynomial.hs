-- | Provides the Polynomial and PolynomialRemainder (used for Polynomial Division) data types.

module Polynomial where

import Variable

data Polynomial = Poly { variables :: [Variable], functionDegree :: Integer} deriving (Show, Eq, Read)
data PolynomialRemainder = PolynomialRatio {numerator :: Polynomial, denominator :: Polynomial} deriving (Show, Eq, Read)