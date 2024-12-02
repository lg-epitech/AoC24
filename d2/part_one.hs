split :: String -> Char -> [String]
split s d =
    case dropWhile (== d) s of
        "" -> []
        s' -> w : split s'' d
            where (w, s'') = break (== d) s'

isSafeReport :: [Int] -> Bool
isSafeReport list
    | all isNegative list = length (filter (>= -3) list) == length list
    | all isPositive list = length (filter (<= 3) list) == length list
    | otherwise = False
    where
        isPositive x = x > 0
        isNegative x = x < 0

main :: IO ()
main = do
    contents <- readFile "d2/input.txt"
    let reports =
            [ [read n :: Int | n <- split line ' ']
            | line <- split contents '\n'
            ]
    let differences =
            filter
                isSafeReport
                [ [ report !! (i + 1) - report !! i
                | i <- [0 .. length report - 2]
                ]
                | report <- reports
                ]
    print (length differences)
