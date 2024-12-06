module Main where

import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x : xs) = (sort [y | y <- xs, y < x]) ++ [x] ++ sort [y | y <- xs, y >= x]

parse :: String -> ([Int], [Int])
parse input = (left, right)
  where
    ids = map words . lines $ input
    left = sort . map (read . head) $ ids
    right = sort . map (read . last) $ ids

solve :: String -> Int
solve = sum . map (abs . uncurry (-)) . uncurry zip . parse

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (== x)

solve2 :: String -> Int
solve2 input = go left right 0
  where
    (left, right) = parse input
    go [] ys acc = acc
    go (x : xs) ys acc = go xs ys (acc + x * count x ys)
