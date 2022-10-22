module Polynomial where

import Data.List
import Data.Char
import Data.String
import Data.Function
import Data.Ord

import Monomial
import Helpers

-- | Type constructor for a polynomial, takes in a list of monomials
newtype Polynomial = Polynomial [Monomial] deriving (Eq)

-- | Derivation operator for a polynomial
instance Derive Polynomial where
  (\/) m v = derivePoly m v

-- | Read instancing for polynomials
instance Read Polynomial where
  readsPrec _ s = [(parseStrPoly s, "")]


-- | Show instancing for polynomials
instance Show Polynomial where
  show (Polynomial s) = showPoly (map showMonom (normPoly s))

-- | Num instancing for polynomials
instance Num Polynomial where
  (+) = addPoly
  (*) = multiplyPoly
  abs = absPoly
  negate = negatePoly
  signum = signumPoly
  fromInteger = fromIntegerPoly

-- | Converts a list of strings into a list of monomials, by converting each one into its respective monomial representation
parsePoly :: [String] -> [Monomial]
parsePoly str = normPoly (map (createMonom . parseMonom) str)

-- | Converts a string into a polynomial
parseStrPoly :: String -> Polynomial
parseStrPoly i = Polynomial (parsePoly (splitByList (i, [], "") ['+']))

-- | Converts a string list, wiht the plain text representation of the monomials of the polyomial, into a polynomial plain text representation of said polynomial
showPoly :: [String] -> String
showPoly [] = ""
showPoly (x:xs) | null xs = x ++ showPoly xs
                | head (head xs) == '-' = x ++ " - " ++ tail (showPoly xs)
                | not (null xs) && x == "" = showPoly xs
                | not (null xs) = x ++ " + " ++ showPoly xs

-- | Sum of a list of monomials, containing lists of monomials by their literals, into a simple list of simplified monomials, equivalent to a polynomial
sum' :: [[Monomial]] -> [Monomial]
sum' = foldr (\ x -> (++) [sum'' x]) []

-- | Sum of a list of monomials, with the same literals, into a single simplified monomial
sum'' :: [Monomial] -> Monomial
sum'' = foldr1 (+)

-- | Normalizes a polynomial
normPoly :: [Monomial] -> [Monomial]
normPoly x =  sum' (group (sort x))

-- | Derivation of a polynomial
derivePoly :: Polynomial -> Var -> Polynomial
derivePoly  (Polynomial l) v = Polynomial (normPoly(map (`deriveMonom` v) l))

-- | Sum of polynomials
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly (Polynomial c) (Polynomial c2) = Polynomial (normPoly (c ++ c2))

-- | Product of a list of monomials, organized in lists of 2 monomials to be multiplied, into a simple list of simplified monomials, equivalent to a polynomial
prod' :: [[Monomial]] -> [Monomial]
prod' = foldr (\ x -> (++) [prod'' x]) []

-- | Product of a list of monomials, returns a single simplified monomial
prod'' :: [Monomial] -> Monomial
prod'' x = foldr1 (*) x

-- | Multiplication of polynomials
multiplyPoly :: Polynomial -> Polynomial -> Polynomial
multiplyPoly (Polynomial c) (Polynomial c2) = Polynomial (normPoly (prod' [[x, y] | x <- c, y <- c2]))

-- | Absolute value of a polynomial
absPoly :: Polynomial -> Polynomial
absPoly (Polynomial c) = Polynomial (map abs c)

-- | Negation value of a polynomial
negatePoly :: Polynomial -> Polynomial
negatePoly (Polynomial p) = Polynomial (map negate p)

-- | Signum of a polynomial
signumPoly :: Polynomial -> Polynomial
signumPoly (Polynomial p) = Polynomial (map signum p)

-- | Intege conversion into a polynomial
fromIntegerPoly :: Integer -> Polynomial
fromIntegerPoly n = Polynomial [Monomial (fromInteger n) [('_',0)]]
