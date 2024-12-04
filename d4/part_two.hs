import Data.List

split :: String -> Char -> [String]
split s d =
    case dropWhile (== d) s of
        "" -> []
        s' -> w : split s'' d
            where (w, s'') = break (== d) s'

xmas :: String
xmas = "XMAS"

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
    | i >= 0 && i < length xs = Just (xs !! i)
    | otherwise = Nothing

safeCheck :: [String] -> Int -> Int -> (Char -> Bool) -> Bool
safeCheck lines y x predicate =
    case (safeIndex lines y, safeIndex (lines !! y) x) of
        (Just _, Just char) -> predicate char
        _ -> False

isCross :: [String] -> Int -> Int -> Int
isCross lines y x
    | safeCheck lines y x (/= 'A') = 0
    | ((safeCheck lines (y - 1) (x - 1) (== 'M') &&
         safeCheck lines (y + 1) (x + 1) (== 'S')) ||
        (safeCheck lines (y - 1) (x - 1) (== 'S') &&
         safeCheck lines (y + 1) (x + 1) (== 'M'))) &&
       ((safeCheck lines (y - 1) (x + 1) (== 'M') &&
         safeCheck lines (y + 1) (x - 1) (== 'S')) ||
        (safeCheck lines (y - 1) (x + 1) (== 'S') &&
         safeCheck lines (y + 1) (x - 1) (== 'M'))) = 1
    | otherwise = 0

countXmas :: [String] -> [[Int]]
countXmas lines =
    [ [isCross lines y x | x <- [0 .. length (lines !! y) - 1]]
    | y <- [0 .. length lines - 1]
    ]

main :: IO ()
main = do
    contents <- readFile "d4/input.txt"
    let lines = split contents '\n'
    print $ sum $ concat $ countXmas lines
