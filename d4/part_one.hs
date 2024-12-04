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

up :: [String] -> Int -> Int -> Int
up lines y x
    | safeCheck lines y x (== 'X') &&
          safeCheck lines (y - 1) x (== 'M') &&
          safeCheck lines (y - 2) x (== 'A') &&
          safeCheck lines (y - 3) x (== 'S') = 1
    | otherwise = 0

upright :: [String] -> Int -> Int -> Int
upright lines y x
    | safeCheck lines y x (== 'X') &&
          safeCheck lines (y - 1) (x + 1) (== 'M') &&
          safeCheck lines (y - 2) (x + 2) (== 'A') &&
          safeCheck lines (y - 3) (x + 3) (== 'S') = 1
    | otherwise = 0

upleft :: [String] -> Int -> Int -> Int
upleft lines y x
    | safeCheck lines y x (== 'X') &&
          safeCheck lines (y - 1) (x - 1) (== 'M') &&
          safeCheck lines (y - 2) (x - 2) (== 'A') &&
          safeCheck lines (y - 3) (x - 3) (== 'S') = 1
    | otherwise = 0

down :: [String] -> Int -> Int -> Int
down lines y x
    | safeCheck lines y x (== 'X') &&
          safeCheck lines (y + 1) x (== 'M') &&
          safeCheck lines (y + 2) x (== 'A') &&
          safeCheck lines (y + 3) x (== 'S') = 1
    | otherwise = 0

downright :: [String] -> Int -> Int -> Int
downright lines y x
    | safeCheck lines y x (== 'X') &&
          safeCheck lines (y + 1) (x + 1) (== 'M') &&
          safeCheck lines (y + 2) (x + 2) (== 'A') &&
          safeCheck lines (y + 3) (x + 3) (== 'S') = 1
    | otherwise = 0

downleft :: [String] -> Int -> Int -> Int
downleft lines y x
    | safeCheck lines y x (== 'X') &&
          safeCheck lines (y + 1) (x - 1) (== 'M') &&
          safeCheck lines (y + 2) (x - 2) (== 'A') &&
          safeCheck lines (y + 3) (x - 3) (== 'S') = 1
    | otherwise = 0

right :: [String] -> Int -> Int -> Int
right lines y x =
    if take (length xmas) (drop x (lines !! y)) == xmas
        then 1
        else 0

left :: [String] -> Int -> Int -> Int
left lines y x =
    if take (length xmas) (drop x (lines !! y)) == reverse xmas
        then 1
        else 0

countXmasPlace :: [String] -> Int -> Int -> Int
countXmasPlace lines y x =
    up lines y x +
    down lines y x +
    right lines y x +
    left lines y x +
    upright lines y x +
    upleft lines y x +
    downright lines y x +
    downleft lines y x

countXmas :: [String] -> [[Int]]
countXmas lines =
    [ [countXmasPlace lines y x | x <- [0 .. length (lines !! y) - 1]]
    | y <- [0 .. length lines - 1]
    ]

main :: IO ()
main = do
    contents <- readFile "d4/input.txt"
    let lines = split contents '\n'
    print $ sum $ concat $ countXmas lines
