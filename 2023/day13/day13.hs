import Data.List (groupBy, transpose)
import Data.Maybe (fromJust, isJust)
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

coincide :: (Eq a) => [a] -> [a] -> Bool
coincide [] _ = True
coincide _ [] = True
coincide (x : xs) (y : ys) = x == y && coincide xs ys

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead xs = Just (head xs)

horizontalReflection :: [String] -> Maybe Int
horizontalReflection xs = maybeHead $ filter (\n -> reverse (take n xs) `coincide` drop n xs) [1 .. length xs - 1]

verticalReflection :: [String] -> Maybe Int
verticalReflection = horizontalReflection . transpose

reflection :: [String] -> Int
reflection xs
  | isJust a = 100 * fromJust a
  | isJust b = fromJust b
  | otherwise = 0
  where
    a = horizontalReflection xs
    b = verticalReflection xs

splitInput :: String -> [[String]]
splitInput input = filter (\xs -> xs /= [""]) $ groupBy (\a b -> a /= "" && b /= "") (lines input)

solve :: String -> Int
solve = sum . map reflection . splitInput

countDiffs :: (Eq a) => [a] -> [a] -> Int
countDiffs = go 0
  where
    go acc [] _ = acc
    go acc _ [] = acc
    go acc (x : xs) (y : ys) = if x == y then go acc xs ys else go (acc + 1) xs ys

almostCoincide :: (Eq a) => [[a]] -> [[a]] -> Bool
almostCoincide = go 0
  where
    go acc [] _ = acc == 1
    go acc _ [] = acc == 1
    go 0 (x : xs) (y : ys)
      | x == y = go 0 xs ys
      | x /= y = go (countDiffs x y) xs ys
    go 1 (x : xs) (y : ys)
      | x == y = go 1 xs ys
      | x /= y = False
    go _ _ _ = False

almostHorizontalReflection :: [String] -> Maybe Int
almostHorizontalReflection xs = maybeHead $ filter (\n -> reverse (take n xs) `almostCoincide` drop n xs) [1 .. length xs - 1]

almostVerticalReflection :: [String] -> Maybe Int
almostVerticalReflection = almostHorizontalReflection . transpose

almostReflection :: [String] -> Int
almostReflection xs
  | isJust a = 100 * fromJust a
  | isJust b = fromJust b
  | otherwise = 0
  where
    a = almostHorizontalReflection xs
    b = almostVerticalReflection xs

solve2 :: String -> Int
solve2 = sum . map almostReflection . splitInput
