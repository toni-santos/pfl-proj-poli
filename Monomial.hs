module Monomial where

import Data.List
import Data.Char
import Data.String
import Data.Function
import Data.Ord

import Helpers

-- | Type constructor for a monomial, uses a coefficient and literals to create a tuple of (Coef, Lits)
data Monomial = Monomial Coef Lits

-- | Derivation operator for a monomial
instance Derive Monomial where
  (\/) m v = deriveMonom m v

-- | Read instancing for monomials
instance Read Monomial where
  readsPrec _ s = [(parseStrMonom s, "")]

-- | Show instancing for monomials
instance Show Monomial where
  show s = if showMonom (normMonom s) == "" then "0" else showMonom (normMonom s)

-- | Equivalence instancing for monomials
instance Eq Monomial where
  (==) (Monomial c l) (Monomial c2 l2) = l == l2

-- | Ordering instancing for monomials
instance Ord Monomial where
  compare (Monomial c l) (Monomial c2 l2) = comparing snd (c, l) (c, l2)

-- | Numerical instancing for monomials
instance Num Monomial where
  (*) = multiplyMonom
  (+) = addMonom
  abs = absMonom
  negate = negateMonom
  signum = signumMonom
  fromInteger = fromInteger

-- | Finds the coefficient of a string representation of a monomial
findMonomCoef :: String -> Coef
findMonomCoef str | takeWhile (\x -> isDigit x || x == '-') str == "" = 1
                  | takeWhile (\x -> isDigit x || x == '-') str == "-" = -1
                  | otherwise = read (takeWhile (\x -> isDigit x || x == '-') str)

-- | Finds the exponents of a string representation of a monomial
findMonomExps :: String -> [Var]
findMonomExps str = filter (\x -> isDigit x && x /= '*' && x /= '^') (dropWhile (\x -> isDigit x || x == '-') str)

-- | Finds the variables of a string representation of a monomial
findMonomVars :: String -> [Var]
findMonomVars = filter (\x -> isAlpha x && x /= '*' && x /= '^')

-- | Finds the literal of a string representation of a monomial
findMonomLit :: String -> [Var]
findMonomLit = dropWhile (\x -> isDigit x || x == '-' || x == '*')

-- | Parses a string representation of a monomial into literals
parseMonomLit :: (String, Lits) -> Lits
parseMonomLit ([], res) = res
parseMonomLit (str, res) = parseMonomLit (
    dropWhile (\x -> isDigit x || x == '^' || x == '*') (tail str),
    res ++ [(
      head str,
      read (
        if null (takeWhile (\x -> isDigit x || x == '^' && x /= '*') (tail str)) then
          "1"
        else
          if head (takeWhile (\x -> isDigit x || x == '^' && x /= '*') (tail str)) == '^' then
            tail (takeWhile (\x -> isDigit x || x == '^' && x /= '*') (tail str))
          else
            takeWhile (\x -> isDigit x || x == '^' && x /= '*') (tail str)
      )
    )]
  )

-- | Parses a string representation of a monomial into a monomial
parseMonom :: String  -> Monomial
parseMonom str = normMonom (Monomial (findMonomCoef str) (if null (parseMonomLit (findMonomLit str, [])) then [('_', 0)] else parseMonomLit (findMonomLit str, [])))

-- | Sums literals
addLits :: Lits -> Lit
addLits l = (fst (head l), sum [y | (x,y) <- l])

-- | Removes useless literals
removeUseless :: Lits -> Lits
removeUseless l = [ (x,y) | (x,y) <- l, (y == 0 && x == '_') || (x/= '_' && y /= 0) ]

-- | Simplifies and fixes literals
fixLits :: Lits -> Lits
fixLits l | l == [('_', 0)] = l
          | otherwise = [ (x,y) | (x,y) <- l, (x,y) /= ('_',0)]

-- | Reduces literals
reduceLits :: Lits -> Lits
reduceLits l = fixLits(removeUseless (map addLits (groupBy ((==) `on` fst) (sortBy (comparing fst) l))))

-- | Normalizes a monomial
normMonom :: Monomial -> Monomial
normMonom (Monomial c l) = Monomial c (reduceLits l)

-- | Correctly creates a monomial, simplifying it when needed
createMonom :: Monomial -> Monomial
createMonom (Monomial c l) = Monomial c ([if y /= 0 then (x,y) else ('_',0) | (x,y) <- l ])

-- | Parses a complex string into a monomial
parseStrMonom :: String -> Monomial
parseStrMonom str = head (map (createMonom . parseMonom) (splitByList (str, [], "") ['+']))

-- | Converts the internal representation of literals into an adequate string
showLits :: Lits -> String
showLits [] = ""
showLits (x:xs) | x == ('_', 0) = showLits xs
                | snd x == 1 = fst x : showLits xs
                | otherwise = [fst x]  ++ "^" ++ show (snd x) ++ showLits xs

-- | Converts the internal representation of a monomial into an adequate string
showMonom :: Monomial -> String
showMonom (Monomial c l) | c == 0 = ""
                         | length l == 1 && fst (head l) /= '_' && l == [(fst (head l), 0)] = ""
                         | length l == 1 && l == [('_', 0)] = show c
                         | c == 1 && (l /= [('_', 0)]) = showLits l
                         | c == -1 = "-" ++ showLits l
                         | notElem c [-1,0,1] && showLits l  == "" = show c
                         | otherwise = show c ++ showLits l

-- | Coefficient calcluation of the derivation operation
calcDervCoef :: Coef -> Lits -> Var -> Coef
calcDervCoef c l v = c * head [ y | (x,y) <- l, x == v]

-- | Literals calcluation of the derivation operation
calcDervLits :: Lits -> Var -> Lits
calcDervLits [] v = []
calcDervLits (x:xs) v | fst x == v && snd x > 1 = [(fst x, (snd x)-1)] ++ calcDervLits xs v
                      | fst x == v && snd x == 1 = [('_', 0)] ++ calcDervLits xs v
                      | otherwise = [(fst x, snd x)] ++ calcDervLits xs v

-- | Derivation of a monomial
deriveMonom :: Monomial -> Var -> Monomial
deriveMonom (Monomial c l) v = if any (\(x,y) -> x == v) l then (Monomial (calcDervCoef c l v) (calcDervLits l v)) else  Monomial 0 [('_',0)]

-- | Sum of monomials
addMonom :: Monomial -> Monomial -> Monomial
addMonom (Monomial c l) (Monomial c2 l2) = Monomial (c Prelude.+ c2) l

-- | Coefficient calcluation of the multiplication operation
calcMultCoef :: Lits -> Lits
calcMultCoef x = map (foldr1 (\(a,b) (c,d) -> (a, b Prelude.+ d))) (groupBy ((==) `on` fst) $ sort x)

-- | Multiplication of monomials
multiplyMonom :: Monomial -> Monomial -> Monomial
multiplyMonom (Monomial c l) (Monomial c2 l2) =  normMonom(Monomial (c Prelude.* c2) (calcMultCoef (l ++ l2)))

-- | Absolute value of a monomial
absMonom :: Monomial -> Monomial
absMonom (Monomial c l) = Monomial (abs c) l

-- | Negation value of a monomial
negateMonom :: Monomial -> Monomial
negateMonom (Monomial c l) = Monomial (negate c) l

-- | Signum of a monomial
signumMonom :: Monomial -> Monomial
signumMonom (Monomial c l) = Monomial (signum c) [('_',0)]

-- | Integer conversion to a monomial
fromIntegerMonom :: Integer -> Monomial
fromIntegerMonom n = Monomial (fromInteger n) [('_',0)]
