module Helpers where

import Data.List
import Data.Char
import Data.String
import Data.Function
import Data.Ord

-- Type aliases for more accurate representation and more readability of monomials/polynomials

type Var = Char
type Exp = Int
type Coef = Int
type Lit = (Var, Exp)
type Lits = [Lit]

-- | Class of the Derivation operation, used so that both Monomial and Polynomial may implement derivation with the same operator
class Derive x where
    (\/) :: x -> Var -> x

-- | Function splitting a string by a list of Chars, returning a list of strings each representing a monomial in plain text.
splitByList :: (String, [String], String) -> String -> [String]
splitByList ([], res, c) a = res ++ [c]
splitByList (x:xs, res, c)  a | x == ' ' || x == '\n' || x == '(' || x == ')' = splitByList (xs, res, c) a
                              | x == '-' && c /= "" = splitByList (xs, res ++ [c], "-") a
                              | x `elem` a = splitByList (xs, res ++ [c], "") a
                              | otherwise = splitByList (xs, res, c ++ [x]) a