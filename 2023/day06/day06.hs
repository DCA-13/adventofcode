import Control.Monad
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

raceDistance :: Int -> Int -> Int
raceDistance t x = x * (t - x)

-- numberWins :: Int -> Int -> Int
-- numberWins t d = 2 * (length . filter (> d) $ map (raceDistance t) [1 .. t `div` 2]) - 1 + t `mod` 2

-- x * (t - x) es creciente si x < t / 2 => solo hace falta buscar el primer x que supere el récord

minChargeTime :: Int -> Int -> Int
minChargeTime t d = aux 0
  where
    aux x = if raceDistance t x > d then x else aux $ x + 1

numberWins :: Int -> Int -> Int
numberWins t d = 2 * (t `div` 2 + 1 - minChargeTime t d) - 1 + t `mod` 2

solve :: String -> Int
solve str = product $ zipWith numberWins time distance
  where
    [time, distance] = map (map read . drop 1 . words) . lines $ str

solve2 :: String -> Int
solve2 str = numberWins time distance
  where
    [time, distance] = map (read . concat . drop 1 . words) . lines $ str
