import Control.Monad
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

diff :: [Int] -> [Int]
diff [] = []
diff [x] = []
diff (x : y : xs) = y - x : diff (y : xs)

predict :: [Int] -> Int
predict [] = 0
predict xs = sum . map last $ iterations
  where
    iterations = takeWhile (any (/= 0)) $ iterate diff xs

solve :: String -> Int
solve = sum . map (predict . map read . words) . lines

alternatingSum :: (Num a) => [a] -> a
alternatingSum = foldr (-) 0

predict2 :: [Int] -> Int
predict2 [] = 0
predict2 xs = alternatingSum . map head $ iterations
  where
    iterations = takeWhile (any (/= 0)) $ iterate diff xs

solve2 :: String -> Int
solve2 = sum . map (predict2 . map read . words) . lines
