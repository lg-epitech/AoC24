import Data.List
import Data.Maybe

type Rule = (Int, Int)

compareWithRules :: [Rule] -> Int -> Int -> Ordering
compareWithRules rules a b =
    let aBeforeB = any (\(x, y) -> x == a && y == b) rules
        bBeforeA = any (\(x, y) -> x == b && y == a) rules
    in case (aBeforeB, bBeforeA) of
    (True, False) -> LT
    (False, True) -> GT
    _ -> compare a b

reorderUpdate :: [Rule] -> [Int] -> [Int]
reorderUpdate rules update =
    let newUpdate = sortBy (compareWithRules rules) update
    in if newUpdate == update
       then update
       else reorderUpdate rules newUpdate

reorderUpdates :: [Rule] -> [[Int]] -> [[Int]]
reorderUpdates rules = map (reorderUpdate rules)

split :: String -> Char -> [String]
split s d =
    case dropWhile (== d) s of
        "" -> []
        s' -> w : split s'' d
            where (w, s'') = break (== d) s'

findDelim :: String -> Maybe Int
findDelim str =
    findIndex (\i -> take 2 (drop i str) == "\n\n") [0 .. length str - 1]

isValidUpdate :: [Rule] -> Int -> [Int] -> Bool
isValidUpdate rules i update
    | i >= length update = True
    | otherwise =
        let previouses = take i update
            invalids = [snd rule | rule <- rules, fst rule == (update !! i)]
        in null (previouses `intersect` invalids) &&
           isValidUpdate rules (i + 1) update

notInvalid :: [Rule] -> Int -> [Int] -> Bool
notInvalid rules i update = not (isValidUpdate rules i update)

filterInvalidUpdates :: [[Int]] -> [Rule] -> [[Int]]
filterInvalidUpdates updates rules = filter (notInvalid rules 0) updates

middles :: [[Int]] -> [Int]
middles updates = [update !! (length update `div` 2) | update <- updates]

main :: IO ()
main = do
    contents <- readFile "d5/input.txt"
    case findDelim contents of
        Just d -> print $ sum $ middles $ map (reorderUpdate rules) $ filterInvalidUpdates updates rules
            where rules =
                      [ (read (head (split line '|')) :: Int, read (last (split line '|')) :: Int)
                      | line <- split (take d contents) '\n'
                      ]
                  updates =
                      [ [read n :: Int | n <- split line ',']
                      | line <- split (drop d contents) '\n'
                      ]
        Nothing -> print "Could not find the delimiter"
