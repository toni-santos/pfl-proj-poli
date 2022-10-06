{-# LANGUAGE DataKinds #-}
import Data.List

combine :: [a] -> [a] -> [[a]]
combine xs ys = [xs,ys]

delim :: (String, [String], String) -> [String]
delim ([], res, c) = res ++ [c]
delim (x:xs, res, c) | x == ' ' || x == '\n' || x == '(' || x == ')' = delim (xs, res, c)
                       | x == '+' = delim (xs, res ++ [c], "")
                       | x == '-' && c /= "" = delim (xs, res ++ [c], "-")
                       | otherwise = delim (xs, res, c ++ [x])

parseMonom :: String -> [String]
parseMonom i = [takeWhile (/= '^') i]-- `intersect` takeWhile (/= '^') i]--, takeWhile (/= '*') i, dropWhile (/= '^') i];

monom :: ([String], [String]) -> [String]
monom ([], res) = res
monom (x:xs, res) = monom (xs, res parseMonom x)

parseInput :: String -> [String]
parseInput input = monom (delim (input, [], ""), [])
