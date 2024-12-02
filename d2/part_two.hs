import Data.List (sort)

isGood :: [Int] -> Bool
isGood xs =
    incOrDec && all (\(a, b) -> 1 <= abs (a - b) && abs (a - b) <= 3) pairs
  where
    sortedXs = sort xs
    incOrDec = xs == sortedXs || xs == reverse sortedXs
    pairs = zip xs (tail xs)

processLine :: Int -> String -> Int
processLine count line = newCount
  where
    xs1 = map read (words line)
    newCount =
        if any (isGood . (\j -> take j xs1 ++ drop (j + 1) xs1))
               [0 .. length xs1 - 1]
            then count + 1
            else count

main :: IO ()
main = do
    content <- readFile "d2/input.txt"
    let linesList = lines content
    print $ foldl processLine 0 linesList
