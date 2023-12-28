import Control.Monad
import Data.Array
import Data.Char
import Data.List
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  -- print . solve2 $ contents
  hClose handle

groups :: String -> [Int]
groups = map length . filter (\x -> nub x == "#") . group

parseNumbers :: String -> [Int]
parseNumbers [] = []
parseNumbers str = read $ "[" ++ str ++ "]"

substitute :: String -> String -> String
substitute str [] = str
substitute [] _ = []
substitute (x : xs) (y : ys) = if x /= '?' then x : substitute xs (y : ys) else y : substitute xs ys

combinations :: a -> a -> Int -> Int -> [[a]]
combinations x _ n 0 = [replicate n x]
combinations _ y 0 m = [replicate m y]
combinations x y n m = map (x :) (combinations x y (n - 1) m) ++ map (y :) (combinations x y n (m - 1))

matches :: String -> Int
matches str = length . filter (\c -> groups c == correctAmounts) $ cases
  where
    [springs, contiguous] = words str
    correctAmounts = parseNumbers contiguous
    perms = nub $ combinations '#' '.' k (n - k)
    n = length . filter (== '?') $ springs
    k = sum correctAmounts - (length . filter (== '#') $ springs)
    cases = map (substitute springs) perms

solve :: String -> Int
solve = sum . map matches . lines
