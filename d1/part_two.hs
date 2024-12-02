import Data.Char
import Data.List

split :: String -> Char -> [String]
split s d =
    case dropWhile (== d) s of
        "" -> []
        s' -> w : split s'' d
            where (w, s'') = break (== d) s'

parseLocations :: String -> [Int]
parseLocations s = [read n :: Int | n <- split s ' ']

count :: [Int] -> Int -> Int
count xs e = length (filter (== e) xs)

main :: IO ()
main = do
    contents <- readFile "d1/input.txt"
    let lines = [parseLocations s | s <- split contents '\n']
    let left = [head l | l <- lines]
    let right = [last l | l <- lines]
    print (sum ([n * count right n | n <- left]))
