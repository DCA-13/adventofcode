module Main where

import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

parse :: String -> [[Int]]
parse = map (map read . words) . lines

increasing :: [Int] -> Bool
increasing (x : y : xs) = x < y && increasing (y : xs)
increasing _ = True

monotone :: [Int] -> Bool
monotone x = increasing x || increasing (reverse x)

differ :: [Int] -> Bool
differ (x : y : xs) = z <= 3 && z > 0 && differ (y : xs)
  where
    z = abs (x - y)
differ _ = True

safe :: [Int] -> Bool
safe x = differ x && monotone x

solve :: String -> Int
solve = length . filter safe . parse

removeOne :: [a] -> [[a]]
removeOne [] = []
removeOne (x : xs) = xs : map (x :) (removeOne xs)

dampener :: [Int] -> Bool
dampener x = any safe (removeOne x)

safe2 :: [Int] -> Bool
safe2 x = safe x || dampener x

solve2 :: String -> Int
solve2 = length . filter safe2 . parse
