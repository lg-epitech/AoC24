import Data.List

split :: String -> Char -> [String]
split s d =
    case dropWhile (== d) s of
        "" -> []
        s' -> w : split s'' d
            where (w, s'') = break (== d) s'

findDelim :: String -> Maybe Int
findDelim str =
    findIndex (\i -> take 2 (drop i str) == "\n\n") [0 .. length str - 1]

isValidUpdate :: [[Int]] -> Int -> [Int] -> Bool
isValidUpdate rules i update =
    (i >= length update) ||
    (let previouses = take i update
         invalids = [last rule | rule <- rules, head rule == (update !! i)]
      in null (previouses `intersect` invalids) &&
         isValidUpdate rules (i + 1) update)

filterValidUpdates :: [[Int]] -> [[Int]] -> [[Int]]
filterValidUpdates updates rules = filter (isValidUpdate rules 0) updates

middles :: [[Int]] -> [Int]
middles updates = [update !! (length update `div` 2) | update <- updates]

main :: IO ()
main = do
    contents <- readFile "d5/input.txt"
    case findDelim contents of
        Just d -> print $ sum $ middles $ filterValidUpdates updates rules
            where rules =
                      [ [read n :: Int | n <- split line '|']
                      | line <- split (take d contents) '\n'
                      ]
                  updates =
                      [ [read n :: Int | n <- split line ',']
                      | line <- split (drop d contents) '\n'
                      ]
        Nothing -> print "Could not find the delimiter"
