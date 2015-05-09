module Main where
	
import Variable 
import Polynomial
import PolynomialArithmetic

main = do 
	putStr 
		(performArithmeticOperationsOnList 
			[ 
				( -- (x^2 + x + 1) and (x + 4)
					Poly {variables = [Var {coefficient = 1, degree = 2}, Var {coefficient = 1, degree = 1}, Var {coefficient = 1, degree = 0}], functionDegree = 2}, 
					Poly {variables = [Var {coefficient = 1, degree = 1}, Var {coefficient = 4, degree = 0}], functionDegree = 1} 
				), 
				( -- (3x^3 - x^2 + 3x - 5) and (x - 1)
					Poly {variables = [Var {coefficient = 3, degree = 3}, Var {coefficient = -1, degree = 2}, Var {coefficient = 3, degree = 1}, Var {coefficient = -5, degree = 0}], functionDegree = 2}, 
					Poly {variables = [Var {coefficient = 1, degree = 1}, Var {coefficient = -1, degree = 0}], functionDegree = 1} 
				) 
			] 
		)

-- Performs all arithmetic operations (add, subtract, multiply, and divide) on all Polynomial pairs in a list.
-- Results are formated for readability and exported into a string.
performArithmeticOperationsOnList :: [(Polynomial, Polynomial)] -> String
performArithmeticOperationsOnList [] = ""
performArithmeticOperationsOnList ((poly1,poly2):list) = performArithmeticOperations poly1 poly2 ++ performArithmeticOperationsOnList list

performArithmeticOperations :: Polynomial -> Polynomial -> String
performArithmeticOperations poly1 poly2 = "\nFunctions:\n" ++ (polynomialToString poly1) ++ "\n" ++ (polynomialToString poly2)
	++ "\n\nAddition:\n" ++ (polynomialToString (addPolynomials poly1 poly2))
	++ "\n\nSubtraction:\n" ++ (polynomialToString (subtractPolynomials poly1 poly2))
	++ "\n\nMultiplication:\n" ++ (polynomialToString (multiplyPolynomials poly1 poly2))
	++ "\n\nDivision:\n" ++ (polynomialToString (fst quotient)) ++ (quotientToString (snd quotient)) 
	++ "\n\n-------------------------------------\n"
	where quotient = dividePolynomials poly1 poly2

-- Translates a Polynomial to a human readable string.
polynomialToString :: Polynomial -> String
polynomialToString Poly {variables = (var:variables)}
	| (coefficient var) == 0 = polynomialToString Poly {variables  = variables, functionDegree = 0}
	| (degree var) == 0 = if (coefficient var) > 0 then "+" ++ show (coefficient var) else show (coefficient var)
	| (coefficient var) == 1.0 = "+x^" ++ show (degree var) ++ " " ++ polynomialToString Poly {variables  = variables, functionDegree = 0}
	| (coefficient var) < 1 = show (coefficient var) ++ "x^" ++ show (degree var) ++ " " ++ polynomialToString Poly {variables  = variables, functionDegree = 0}
	| otherwise = "+" ++ show (coefficient var) ++ "x^" ++ show (degree var) ++ " " ++ polynomialToString Poly {variables  = variables, functionDegree = 0}

-- Translates a PolynomialRemainder (generated in division) to a human readable string.
quotientToString :: PolynomialRemainder -> String
quotientToString PolynomialRatio {
	numerator = Poly {variables = [], functionDegree = 0}, 
	denominator = Poly {variables = [], functionDegree = 0}} =
	""
quotientToString PolynomialRatio {numerator = num, denominator = den} =
	" + ( " ++ (polynomialToString num) ++ 
	" ) / ( " ++ (polynomialToString den) ++
	" )"