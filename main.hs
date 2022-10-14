import Data.List
import Data.Char
import Data.String
import Text.Parsec (parse)

delim :: (String, [String], String) -> [Char] -> [String]
delim ([], res, c) a = res ++ [c]
delim (x:xs, res, c)  a | x == ' ' || x == '\n' || x == '(' || x == ')' = delim (xs, res, c) a
                        | x == '-' && c /= "" = delim (xs, res ++ [c], "-") a
                        | x `elem` a = delim (xs, res ++ [c], "") a
                        | otherwise = delim (xs, res, c ++ [x]) a

findMonomCoef :: String -> Int
findMonomCoef str =  if str == "-" then -1 else read (takeWhile (\x -> isDigit x || x == '-') str)

findMonomExps :: String -> [Char]
findMonomExps str = filter (\x -> isDigit x && x /= '*' && x /= '^') (dropWhile (\x -> isDigit x || x == '-') str)

findMonomVars :: String -> [Char]
findMonomVars = filter (\x -> isAlpha x && x /= '*' && x /= '^')

findMonomLit :: String -> String
findMonomLit = dropWhile (\x -> isDigit x || x == '-' || x == '*')

parseMonomLit :: (String, [(Char, Int)]) -> [(Char, Int)] -- x2y2 x xy2 x2y xy
parseMonomLit ([], res) = res
parseMonomLit (str, res) = parseMonomLit (dropWhile (\x -> isDigit x || x == '^' || x == '*') (tail str), res ++ [(head str, read (
    if null (takeWhile isDigit (dropWhile (\x -> isAlpha x || x == '^' || x == '*') str)) then "1" else takeWhile isDigit (dropWhile (\x -> isAlpha x || x == '^' || x == '*') str)
    ))])

parseMonom :: String -> (Int, [(Char, Int)])
parseMonom str = (findMonomCoef str, if null (parseMonomLit (findMonomLit str, [])) then [('_', 0)] else parseMonomLit (findMonomLit str, []))

parsePolis :: [String] -> [(Int, [(Char, Int)])]
parsePolis = map parseMonom

parseStr :: String -> [String]
parseStr i = delim (i, [], "") ['+']

parseInput :: String -> [(Int, [(Char, Int)])]
parseInput input = parsePolis (parseStr input)