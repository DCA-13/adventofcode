import Control.Monad
import Data.Map qualified as M
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

type Space = M.Map (Int, Int) Char

n2 = map (\n -> map (n,) [1 ..]) [1 ..]

parseInput :: String -> Space
parseInput = M.fromAscList . concat . zipWith zip n2 . lines

bounds :: Space -> [Int]
bounds space = [u, d, l, r]
  where
    u = minimum . M.mapWithKey (\k _ -> fst k) $ space
    d = maximum . M.mapWithKey (\k _ -> fst k) $ space
    l = minimum . M.mapWithKey (\k _ -> snd k) $ space
    r = maximum . M.mapWithKey (\k _ -> snd k) $ space

expandedRows :: Space -> [Int]
expandedRows space = filter (\n -> all (== '.') (M.filterWithKey (\k a -> fst k == n) space)) [u .. d]
  where
    [u, d, _, _] = bounds space

expandedCols :: Space -> [Int]
expandedCols space = filter (\n -> all (== '.') (M.filterWithKey (\k a -> snd k == n) space)) [l .. r]
  where
    [_, _, l, r] = bounds space

distance :: [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
distance expRows expCols (x, y) (z, w) = abs (x - z) + abs (y - w) + a + b
  where
    a = length . filter (\n -> x < n && n < z || z < n && n < x) $ expRows
    b = length . filter (\n -> y < n && n < w || w < n && n < y) $ expCols

solve :: String -> Int
solve input = sum . map (uncurry (distance expRows expCols)) $ [(a, b) | a <- galaxies, b <- galaxies, a < b]
  where
    space = parseInput input
    galaxies = M.keys . M.filter (== '#') $ space
    expRows = expandedRows space
    expCols = expandedCols space

olderDistance :: [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
olderDistance expRows expCols (x, y) (z, w) = abs (x - z) + abs (y - w) + (1000000 - 1) * a + (1000000 - 1) * b
  where
    a = length . filter (\n -> x < n && n < z || z < n && n < x) $ expRows
    b = length . filter (\n -> y < n && n < w || w < n && n < y) $ expCols

solve2 :: String -> Int
solve2 input = sum . map (uncurry (olderDistance expRows expCols)) $ [(a, b) | a <- galaxies, b <- galaxies, a < b]
  where
    space = parseInput input
    galaxies = M.keys . M.filter (== '#') $ space
    expRows = expandedRows space
    expCols = expandedCols space
