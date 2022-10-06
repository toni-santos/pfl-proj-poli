delim :: (String, [String], String) -> [[Char]]
delim ([], res, c) = res ++ [c]
delim ((x:xs), res, c) | x == ' ' || x == '\n' || x == '(' || x == ')' = delim (xs, res, c)
                        | x == '+' = delim (xs, res ++ [c], "")
                        | x == '-' && c /= "" = delim (xs, res ++ [c], "-")
                        | otherwise = delim (xs, res, c ++ [x])

parseInput :: String -> [String]
parseInput input = delim (input, [], "")
