{-# LANGUAGE DataKinds, PolyKinds #-}

import Data.List
import Data.Char
import Data.String
import GHC.Generics
import Data.Bifunctor
import Data.Kind

type Var = Char
type Exp = Int
type Coef = Int
type Lit = (Var, Exp)
type Lits = [Lit]

data Monomial x y = Monomial x [y] deriving (Eq, Show)

instance Bifunctor Monomial where
  bimap func func' (x, y) = (func x, func' y)
  first func (x, y) = (func x, y)
  second func (x, y) = (x, func y)

newtype Polynomial x = Polynomial [x] deriving (Eq, Show)

instance Functor Polynomial where
  
{-instance Num Monomial where
   (+) = addMonom
   (*) = multiplyMonom
   abs = abs
   signum = signum
   fromInteger = fromInteger
   negate = negate-}

-- instance Num Polynomial where
--   (+) = addPoly
--   (*) = multiplyPoly
--   abs = _You can define
--   signum = _
--   fromInteger = _
--   negate = _


splitByList :: (String, [String], String) -> [Char] -> [String]
splitByList ([], res, c) a = res ++ [c]
splitByList (x:xs, res, c)  a | x == ' ' || x == '\n' || x == '(' || x == ')' = splitByList (xs, res, c) a
                        | x == '-' && c /= "" = splitByList (xs, res ++ [c], "-") a
                        | x `elem` a = splitByList (xs, res ++ [c], "") a
                        | otherwise = splitByList (xs, res, c ++ [x]) a

findMonomCoef :: String -> Coef
findMonomCoef str =  if str == "-" then -1 else read (takeWhile (\x -> isDigit x || x == '-') str)

findMonomExps :: String -> [Var]
findMonomExps str = filter (\x -> isDigit x && x /= '*' && x /= '^') (dropWhile (\x -> isDigit x || x == '-') str)

findMonomVars :: String -> [Var]
findMonomVars = filter (\x -> isAlpha x && x /= '*' && x /= '^')

findMonomLit :: String -> [Var]
findMonomLit = dropWhile (\x -> isDigit x || x == '-' || x == '*')

parseMonomLit :: (String, Lits) -> Lits
parseMonomLit ([], res) = res
parseMonomLit (str, res) = parseMonomLit ( dropWhile (\x -> isDigit x || x == '^' || x == '*') (tail str), res ++ [(head str, read (
    if null (takeWhile isDigit (dropWhile (\x -> isAlpha x || x == '^' || x == '*') str)) then "1" else takeWhile isDigit (dropWhile (\x -> isAlpha x || x == '^' || x == '*') str)
    ))])

parseMonom :: Monomial res => String -> Monomial
parseMonom str = Monomial (findMonomCoef str) (if null (parseMonomLit (findMonomLit str, [])) then [('_', 0)] else parseMonomLit (findMonomLit str, []))

parsePolis :: [String] -> [Monomial]
parsePolis = map parseMonom

parseStr :: String -> [String]
parseStr i = splitByList (i, [], "") ['+']

-- addPoly :: Polynomial -> Polynomial -> Polynomial
-- addPoly = zipWith (+)

-- addMonom :: Monomial -> Monomial -> Monomial
-- addMonom a b = if snd a == snd b then (fst a + fst b, snd a) else 0;

-- multiplyPoly :: Polynomial -> Polynomial -> Polynomial
-- multiplyPoly a b = zipWith (*)

-- unMonom :: Monomial -> (Coef, Lits)
-- unMonom a = fst a

-- multiplyMonom :: Monomial -> Monomial -> Monomial
-- multiplyMonom a b = Monomial (fst a * fst b)findMonomCoef str