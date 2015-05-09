-- | Provides functions that perform an arithmetic operation on two Polynomials.

module PolynomialOperations where

import Variable
import Polynomial


-- ------ Add ------
-- Notes: Assumes both polynomial functions are in decreasing order of degree, such as: x^2 + x + 1. Uses tail recursion.
--        addPolynomials uses addPolynomialVariables to simplify the process by eliminating the need to constantly construct Polynomial items.
-- -----------------
addPolynomials :: Polynomial -> Polynomial -> Polynomial
addPolynomials poly1 poly2 = Poly {variables = sum, functionDegree = degree (head sum)}
		where sum = addPolynomialVariables (variables poly1) (variables poly2) []

addPolynomialVariables :: [Variable] -> [Variable] -> [Variable] -> [Variable] -- addPolynomialVariables f(x) f(x) new f(x)
addPolynomialVariables [] (head2:variables2) newVariableList = (reverse newVariableList) ++ (head2:variables2) -- 0 + f(x) = newPolynomial + f(x)
addPolynomialVariables (head1:variables1) [] newVariableList = (reverse newVariableList) ++ (head1:variables1) -- f(x) + 0 = newPolynomial + f(x)
addPolynomialVariables [] [] newVariableList = (reverse newVariableList) -- 0 + 0 = newPolynomial
addPolynomialVariables (head1:variables1) (head2:variables2) newVariableList -- x^a + x^b
	| (degree head1) > (degree head2) = addPolynomialVariables variables1 (head2:variables2) (head1:newVariableList) -- a > b
	| (degree head1) < (degree head2) = addPolynomialVariables (head1:variables1) variables2 (head2:newVariableList) -- a < b
	| otherwise = if (coefficient head1) + (coefficient head2) == 0 -- a == b
	  then
		addPolynomialVariables variables1 variables2 newVariableList
	  else
	  	addPolynomialVariables variables1 variables2 ((Var {coefficient = (coefficient head1) + (coefficient head2), degree = (degree head1)}):newVariableList)


-- ------ Subtract ------
-- Notes: Simply adds the negation of the second polynomial function.
--        subtractPolynomials uses subtractPolynomialVariables to simplify the process by eliminating the need to constantly construct Polynomial items.
-- ----------------------
subtractPolynomials :: Polynomial -> Polynomial -> Polynomial
subtractPolynomials poly1 poly2  = addPolynomials poly1 (negatePolynomial poly2)

negatePolynomial :: Polynomial -> Polynomial
negatePolynomial Poly {variables = variables1, functionDegree = functionDegree1} = Poly {variables = map 
	(\(Var {coefficient = coefficient1, degree = degree1}) -> Var {coefficient = -coefficient1, degree = degree1}) 
	variables1, 
functionDegree = functionDegree1}

negatePolynomialVariables :: [Variable] -> [Variable]
negatePolynomialVariables variables = map 
	(\(Var {coefficient = coefficient1, degree = degree1}) -> Var {coefficient = -coefficient1, degree = degree1}) 
	variables

subtractPolynomialVariables :: [Variable] -> [Variable] -> [Variable]
subtractPolynomialVariables variables1 variables2 = addPolynomialVariables variables1 (negatePolynomialVariables variables2) []


-- ------ Multiply ------
-- Notes: Uses a recursive method of multiplication that has a O(N*M) time complexity (not including the concatenation!)
--        The algorithm multiplies the terms one by one. Instead of simple concatenating every new resulting term, they are added using addPolynomialVariables.
--        This automatically combines similar terms, which may not be entirely necessary, but it would be unusual not to.
--        multiplyPolynomials uses multiplyPolynomialVariables to simplify the process by eliminating the need to constantly construct Polynomial items.
-- ----------------------
multiplyPolynomials :: Polynomial -> Polynomial -> Polynomial
multiplyPolynomials poly1 poly2 
	| (functionDegree poly1) == 0 || (functionDegree poly2) == 0 = zeroPolynomial
	| otherwise = Poly {variables = multiplyPolynomialVariables (variables poly1) (variables poly2), functionDegree = (functionDegree poly1 + functionDegree poly2)}

multiplyPolynomialVariables :: [Variable] -> [Variable] -> [Variable]
multiplyPolynomialVariables [] _ = []
multiplyPolynomialVariables _ [] = []
multiplyPolynomialVariables (var1:variables1) variables2 = addPolynomialVariables 
	(map (\var2 -> multiplyTwoVariables var1 var2) variables2) 
	(multiplyPolynomialVariables variables1 variables2) 
	[]
-- addPolynomialVariables is used instead of a list concatenation "++" so similar terms are combined. May increase time complexity, but increases overall simplicty

multiplyTwoVariables :: Variable -> Variable -> Variable
multiplyTwoVariables var1 var2 = Var {coefficient = (coefficient var1) * (coefficient var2), degree = (degree var1) + (degree var2)}


-- ------ Divide ------
-- Notes: Uses a tail recursive method of dividing the first polynomial by the second. Notes are provided on each function to explain their functionality.
--        dividePolynomials uses dividePolynomialVariables to simplify the process by eliminating the need to constantly construct Polynomial items.
-- ----------------------
dividePolynomials :: Polynomial -> Polynomial -> (Polynomial, PolynomialRemainder)
-- Notes: This function returns a pair that holds the resulting polynomial, and a remiander polynomial. If the remainder is 0, the PolynomialRemainder will consist of a Polynomial with 0 variables and 0 degree.
dividePolynomials dividend divisor 
	| (functionDegree divisor) > (functionDegree dividend) = (zeroPolynomial, PolynomialRatio {numerator = dividend, denominator = divisor})
	| otherwise = (Poly {variables = resultingVariables, functionDegree = if resultingVariables == [] then 0 else degree (head resultingVariables)}, resultingRemainder)
	where (resultingVariables, resultingRemainder) = dividePolynomialVariables (variables dividend) (variables divisor) []

divideTwoVariables :: Variable -> Variable -> Variable
-- Notes: This returns a tuple with a list of the resulting Polynomial variables, and a PolynomialRemainder.
divideTwoVariables variable1 variable2  
	| (coefficient variable2) == 0 = Var {coefficient = 0, degree = 0} --UNDEFINED
	| (coefficient variable1) == 0 = Var {coefficient = 0, degree = 0}
	| (degree variable2) > (degree variable1) = Var {coefficient = 0, degree = 0}
	| otherwise = Var {coefficient = (coefficient variable1) / (coefficient variable2), degree = (degree variable1) - (degree variable2)}

dividePolynomialVariables :: [Variable] -> [Variable] -> [Variable] -> ([Variable], PolynomialRemainder)
-- Notes: This function looks a little messy. It follows the standard long division algorithm of polynomial division, so its not as efficient as it could be.
dividePolynomialVariables [] poly2 result = (reverse result, zeroPolynomialRemainder)
dividePolynomialVariables poly1 [] result = ([], zeroPolynomialRemainder) -- UNDEFINED
dividePolynomialVariables (dividendHead:dividendTail) (divisorHead:divisorTail) result
	| (coefficient dividendHead) == 0 = (result, zeroPolynomialRemainder)
	| otherwise = case aTerm of
			Var {coefficient = 0, degree = 0} -> (reverse result, PolynomialRatio {
					numerator = Poly {variables = (dividendHead:dividendTail), functionDegree = degree dividendHead}, 
					denominator = Poly {variables = (divisorHead:divisorTail), functionDegree = degree divisorHead}
					})
			_ -> dividePolynomialVariables (subtractPolynomialVariables 
					(dividendHead:dividendTail) 
					(map (\divisorTerm -> (multiplyTwoVariables aTerm divisorTerm)) (divisorHead:divisorTail))
					) 
				(divisorHead:divisorTail) (aTerm:result)
	where aTerm = divideTwoVariables dividendHead divisorHead

zeroPolynomialRemainder = PolynomialRatio {numerator = zeroPolynomial, denominator = zeroPolynomial}

zeroPolynomial = Poly {variables = [], functionDegree = 0}







-- ------------------* Addition and Division that uses Polynomials as arguments *-----------------
-- These two functions are identical to the two functions above except they take Polynomails as agruments.
-- This is how they were originally, bu the code looked too messy for my taste becuase of the constant constructing of Polynomials for each recursive call.



--dividePolynomials (Poly {variables = []}) poly2 result=
--	Poly {variables = result, functionDegree = if result == [] then 0 else degree (head result)}
--dividePolynomials poly1 (Poly {variables = []}) result=
--	Poly {variables = [], functionDegree = 0} -- UNDEFINED
--dividePolynomials poly1 Poly {functionDegree = 0} _ = poly1 -- UNDEFINED
--dividePolynomials (Poly {variables = (dividendHead:dividendTail), functionDegree = dividendDegree}) (Poly {variables = (divisorHead:divisorTail), functionDegree = divisorDegree}) result
--	| divisorDegree > dividendDegree = Poly {variables = [], functionDegree = 0}
--	| (coefficient dividendHead) == 0 = Poly {variables = result, functionDegree = degree (head result)} -- ADD REMAINDER
--	| otherwise = case aTerm of
--			Var {coefficient = 0, degree = 0} -> addPolynomials (dividePolynomials Poly { variables = (dividendHead:dividendTail), functionDegree = (degree dividendHead) } Poly { variables = (divisorHead:dividendTail), functionDegree = (degree divisorHead) } []) Poly {variables = result, functionDegree = degree (head result)} -- Result + (Remainder/Divisor)
--			_ -> dividePolynomials (subtractPolynomials Poly {variables = (dividendHead:dividendTail), functionDegree = dividendDegree} Poly {variables = (map (\divisorTerm -> (multiplyTwoVariables aTerm divisorTerm)) (divisorHead:divisorTail)), functionDegree = divisorDegree})  Poly {variables = (divisorHead:divisorTail), functionDegree = divisorDegree}  (aTerm:result)
--	where aTerm = (divideTwoVariables dividendHead divisorHead)



-- addPolynomials f(x) f(x) new f(x)
--addPolynomials (Poly {variables = []}) poly2 newVariableList = Poly {variables = (reverse newVariableList) ++ (variables poly2), functionDegree = (degree (head (reverse newVariableList)))} -- 0 + f(x) = newPolynomial + f(x)
--addPolynomials poly1 (Poly {variables = []}) newVariableList = Poly {variables = (reverse newVariableList) ++ (variables poly1), functionDegree = (degree (head (reverse newVariableList)))} -- f(x) + 0 = newPolynomial + f(x)
--addPolynomials (Poly {variables = []}) (Poly {variables = []}) newVariableList = Poly {variables = (reverse newVariableList), functionDegree = (degree (head (reverse newVariableList)))} -- 0 + 0 = newPolynomial
--addPolynomials (Poly {variables = (head1:tail1)}) (Poly {variables = (head2:tail2)}) newVariableList -- x^a + x^b
--	| (degree head1) > (degree head2)  = addPolynomials (Poly {variables = tail1}) (Poly {variables = (head2:tail2)}) (head1:newVariableList) -- a > b
--	| (degree head1) < (degree head2)  = addPolynomials (Poly {variables = (head1:tail1)}) (Poly {variables = tail2}) (head2:newVariableList) -- a < b
--	| otherwise = if (coefficient head1) + (coefficient head2) == 0 then -- a == b
--		addPolynomials (Poly {variables = tail1}) (Poly {variables = tail2}) newVariableList
--	  else
--	  	addPolynomials (Poly {variables = tail1}) (Poly {variables = tail2}) ((Var {coefficient = (coefficient head1) + (coefficient head2), degree = (degree head1)}):newVariableList)


