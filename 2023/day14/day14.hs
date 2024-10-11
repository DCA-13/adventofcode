import Data.List (elemIndex, sortBy, transpose)
import Data.Maybe (fromJust, isJust)
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

-- rockOrder :: Char -> Char -> Ordering
-- rockOrder '.' 'O' = GT
-- rockOrder 'O' '.' = LT
-- rockOrder _ _ = EQ

rollLeft :: String -> String
rollLeft = go 0 []
  where
    go changes acc [x] = go changes (acc ++ [x]) []
    go changes acc [] = if changes == 0 then acc else go 0 [] acc
    go changes acc (x : y : xs) =
      if x == '.' && y == 'O'
        then go (changes + 1) (acc ++ [y]) (x : xs)
        else go changes (acc ++ [x]) (y : xs)

rollNorth :: [String] -> [String]
rollNorth = transpose . map rollLeft . transpose

countLoad :: [String] -> Int
countLoad xs = go 0 0 xs
  where
    go :: Int -> Int -> [String] -> Int
    go count sum [] = sum
    go count sum (x : xs) = go (count + current) (sum + count + current) xs
      where
        current = length $ filter (== 'O') x

solve :: String -> Int
solve = countLoad . rollNorth . lines

rollWest :: [String] -> [String]
rollWest = map rollLeft

rollEast :: [String] -> [String]
rollEast = map reverse . rollWest . map reverse

rollSouth :: [String] -> [String]
rollSouth = reverse . rollNorth . reverse

rollCycle :: [String] -> [String]
rollCycle = rollEast . rollSouth . rollWest . rollNorth

findOrder :: [String] -> (Int, Int)
findOrder xs = go [xs] xs
  where
    go acc xs = if isJust offset' then (offset, order) else go (acc ++ [newCycle]) newCycle
      where
        newCycle = rollCycle xs
        offset' = elemIndex newCycle acc
        offset = fromJust offset'
        order = length acc - offset

solve2 :: String -> Int
solve2 xs = countLoad $ iterate rollCycle (lines xs) !! reducedCycles
  where
    (offset, order) = findOrder $ lines xs
    reducedCycles = ((1000000000 - offset) `mod` order) + offset
