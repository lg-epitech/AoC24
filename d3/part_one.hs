import Data.Char
import Data.List
import Text.Read (readMaybe)

parseNumberTo :: String -> Char -> Int
parseNumberTo s d
    | all isDigit numStr && not (null numStr) =
        case readMaybe numStr :: Maybe Int of
            Just n ->
                if n <= 999
                    then n
                    else 0
            Nothing -> 0
    | otherwise = 0
  where
    numStr = takeWhile (/= d) s

parseMult :: (String, Int) -> (String, Int)
parseMult (memory, c) = (nextMemory, newCount)
  where
    nextMemory = drop (length "mul(") memory
    left = parseNumberTo nextMemory ','
    right =
        case left of
            0 -> 0
            _ -> parseNumberTo (drop 1 (dropWhile (/= ',') nextMemory)) ')'
    newCount = c + (left * right)

analyse :: String -> Int -> Int
analyse "" c = c
analyse memory c
    | "mul(" `isPrefixOf` memory = analyse memory' c'
    | otherwise = analyse (drop 1 memory) c
  where
    (memory', c') = parseMult (memory, c)

main :: IO ()
main = do
    memory <- readFile "d3/input.txt"
    print $ analyse memory 0
