import Data.List
import Data.Char
import Data.String
import Data.Function
import Data.Ord

type Var = Char
type Exp = Int
type Coef = Int
type Lit = (Var, Exp)
type Lits = [Lit]

class Derive x where
  (\/) :: x -> Var -> x

data Monomial = Monomial Coef Lits

newtype Polynomial = Polynomial [Monomial] deriving (Eq)

instance Derive Monomial where
  (\/) m v = deriveMonom m v

instance Derive Polynomial where
  (\/) m v = derivePoli m v

instance Read Monomial where
  readsPrec _ s = [(parseStrMonom s, "")]

instance Read Polynomial where
  readsPrec _ s = [(parseStrPolis s, "")]

instance Show Monomial where
  show s = showMonom (normMonom s)

instance Show Polynomial where
  show (Polynomial s) = showPoli (map showMonom (normPolis s))

instance Eq Monomial where
  (==) (Monomial c l) (Monomial c2 l2) = l == l2

instance Ord Monomial where
  compare (Monomial c l) (Monomial c2 l2) = comparing snd (c, l) (c, l2)

instance Num Monomial where
  (*) = multiplyMonom
  (+) = addMonom
  abs = absMonom
  negate = negateMonom
  signum = signumMonom
  fromInteger = fromInteger

instance Num Polynomial where
  (+) = addPoly
  (*) = multiplyPoly
  abs = absPoly
  negate = negatePoly
  signum = signumPoly
  fromInteger = fromIntegerPoly

splitByList :: (String, [String], String) -> String -> [String]
splitByList ([], res, c) a = res ++ [c]
splitByList (x:xs, res, c)  a | x == ' ' || x == '\n' || x == '(' || x == ')' = splitByList (xs, res, c) a
                              | x == '-' && c /= "" = splitByList (xs, res ++ [c], "-") a
                              | x `elem` a = splitByList (xs, res ++ [c], "") a
                              | otherwise = splitByList (xs, res, c ++ [x]) a

findMonomCoef :: String -> Coef
findMonomCoef str | takeWhile (\x -> isDigit x || x == '-') str == "" = 1
                  | takeWhile (\x -> isDigit x || x == '-') str == "-" = -1
                  | otherwise = read (takeWhile (\x -> isDigit x || x == '-') str)

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

parseMonom :: String  -> Monomial
parseMonom str = normMonom (Monomial (findMonomCoef str) (if null (parseMonomLit (findMonomLit str, [])) then [('_', 0)] else parseMonomLit (findMonomLit str, [])))

addLits :: Lits -> Lit
addLits l = (fst (head l), sum [y | (x,y) <- l])

showLits :: Lits -> String
showLits [] = ""
showLits (x:xs) | fst x == '_' = ""
                | snd x == 0 = showLits xs
                | snd x == 1 = fst x : showLits xs
                | otherwise = [fst x]  ++ "^" ++ show (snd x) ++ showLits xs

showMonom :: Monomial -> String
showMonom (Monomial c l) | c == 0 = ""
                         | c == 1 = showLits l
                         | c == -1 = "-" ++ showLits l
                         | notElem c [-1,0,1] && showLits l == "" = show c
                         | otherwise = show c ++ "*" ++ showLits l

showPoli :: [String] -> String
showPoli [] = ""
showPoli (x:xs) | null xs = x ++ showPoli xs
                | head (head xs) == '-' = x ++ " - " ++ tail (showPoli xs)
                | not (null xs) && x == "" = showPoli xs
                | not (null xs) = x ++ " + " ++ showPoli xs

removeUseless :: Lits -> Lits
removeUseless l = [ (x,y) | (x,y) <- l, y /= 0]

reduceLits :: Lits -> Lits
reduceLits l = removeUseless (map addLits (groupBy ((==) `on` fst) (sortBy (comparing fst) l)))

normMonom :: Monomial -> Monomial
normMonom (Monomial c l) = Monomial c (reduceLits l)

normPolis :: [Monomial] -> [Monomial]
normPolis x =  sum' (group (sort x))

createMonoms :: Monomial -> Monomial
createMonoms (Monomial c l) = Monomial c ([if y /= 0 then (x,y) else ('_',0) | (x,y) <- l ])

parsePolis :: [String] -> [Monomial]
parsePolis str = normPolis (map (createMonoms . parseMonom) str)

parseStrPolis :: String -> Polynomial
parseStrPolis i = Polynomial (parsePolis (splitByList (i, [], "") ['+']))

parseStrMonom :: String -> Monomial
parseStrMonom str = head (map (createMonoms . parseMonom) (splitByList (str, [], "") ['+']))

calcDervCoef :: Coef -> Lits -> Coef
calcDervCoef c l = c * product [ y | (x,y) <- l]

calcDervLits :: Lits -> Lits
calcDervLits l = [if y == 0 then ('_', 0) else (x,y) | (x,y) <- zip [x | (x,y) <- l] [y-1 | (x,y) <- l]]

deriveMonom :: Monomial -> Var -> Monomial
deriveMonom (Monomial c l) v = if any (\(x,y) -> x == v) l then (Monomial (calcDervCoef c l) (calcDervLits l)) else  Monomial 0 [('_',0)]

derivePoli :: Polynomial -> Var -> Polynomial
derivePoli  (Polynomial l) v = Polynomial (map (`deriveMonom` v) l)

sum' :: [[Monomial]] -> [Monomial]
sum' = foldr (\ x -> (++) [sum'' x]) []

sum'' :: [Monomial] -> Monomial
sum'' = foldr1 (+)

addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly (Polynomial c) (Polynomial c2) = Polynomial (normPolis (c ++ c2))

addMonom :: Monomial -> Monomial -> Monomial
addMonom (Monomial c l) (Monomial c2 l2) = Monomial (c Prelude.+ c2) l

prod' :: [[Monomial]] -> [Monomial]
prod' = foldr (\ x -> (++) [prod'' x]) []

prod'' :: [Monomial] -> Monomial
prod'' = foldr1 (*)

multiplyPoly :: Polynomial -> Polynomial -> Polynomial
multiplyPoly (Polynomial c) (Polynomial c2) = Polynomial (prod' [[x, y] | x <- c, y <- c2])

calcMultCoef :: Lits -> Lits
calcMultCoef x = map (foldr1 (\(a,b) (c,d) -> (a, b Prelude.+ d))) (groupBy ((==) `on` fst) $ sort x)

multiplyMonom :: Monomial -> Monomial -> Monomial
multiplyMonom (Monomial c l) (Monomial c2 l2) =  Monomial (c Prelude.* c2) (calcMultCoef (l ++ l2))

absMonom :: Monomial -> Monomial
absMonom (Monomial c l) = Monomial (abs c) l

absPoly :: Polynomial -> Polynomial
absPoly (Polynomial c) = Polynomial (map abs c)

negateMonom :: Monomial -> Monomial
negateMonom (Monomial c l) = Monomial (negate c) l

negatePoly :: Polynomial -> Polynomial
negatePoly (Polynomial p) = Polynomial (map negate p)

signumMonom :: Monomial -> Monomial
signumMonom (Monomial c l) = Monomial (signum c) [('_',0)]

signumPoly :: Polynomial -> Polynomial
signumPoly (Polynomial p) = Polynomial (map signum p)

fromIntegerMonom :: Integer -> Monomial
fromIntegerMonom n = Monomial (fromInteger n) [('_',0)]

fromIntegerPoly :: Integer -> Polynomial
fromIntegerPoly n = Polynomial [Monomial (fromInteger n) [('_',0)]]
