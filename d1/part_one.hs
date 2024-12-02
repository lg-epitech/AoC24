import Data.Char
import Data.List

split :: String -> Char -> [String]
split s d =
    case dropWhile (== d) s of
        "" -> []
        s' -> w : split s'' d
            where (w, s'') = break (== d) s'

parseLocations :: String -> [Integer]
parseLocations s = [read n :: Integer | n <- split s ' ']

main :: IO ()
main = do
    contents <- readFile "d1/input.txt"
    let lines = [parseLocations s | s <- split contents '\n']
    let left = sort [head l | l <- lines]
    let right = sort [last l | l <- lines]
    print (sum ([abs (left !! i - right !! i) | i <- [0 .. length left - 1]]))
