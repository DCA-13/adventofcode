module Main where

import Data.Array
import Data.Set qualified as Set
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

data Dir = U | D | L | R deriving (Show, Eq, Ord)

newtype GridEntry = GE [(Dir, Dir)] deriving (Show, Eq)

type Grid = Array (Int, Int) GridEntry

parseInput :: [String] -> Grid
parseInput xs =
  array ((0, 0), (n, m))
    . concat
    . zipWith (\i s -> map (\(j, x) -> ((i, j), x)) s) [0 ..]
    . map (zip [0 ..] . map parseEntry)
    $ xs
  where
    parseEntry '.' = GE [(U, U), (D, D), (L, L), (R, R)]
    parseEntry '/' = GE [(R, U), (U, R), (D, L), (L, D)]
    parseEntry '\\' = GE [(R, D), (D, R), (U, L), (L, U)]
    parseEntry '|' = GE [(R, U), (R, D), (L, U), (L, D), (U, U), (D, D)]
    parseEntry '-' = GE [(U, L), (U, R), (D, L), (D, R), (R, R), (L, L)]
    n = length xs - 1
    m = length (head xs) - 1

type Beam = ((Int, Int), Dir)

numDir :: Dir -> (Int, Int)
numDir U = (-1, 0)
numDir D = (1, 0)
numDir L = (0, -1)
numDir R = (0, 1)

step :: Grid -> Beam -> [Beam]
step grid ((i, j), dir) = ws
  where
    GE xs = grid ! (i, j)
    ys = map snd . filter (\x -> fst x == dir) $ xs
    zs = map (\dir -> ((i + fst (numDir dir), j + snd (numDir dir)), dir)) ys
    ws = filter (\((i, j), _) -> i >= 0 && j >= 0 && i <= n && j <= m) zs
    ((_, _), (n, m)) = bounds grid

solve' :: Beam -> Grid -> Int
solve' start = go Set.empty [start]
  where
    go seen beams grid =
      if null nubBeams
        then energized
        else go newSeen newBeams grid
      where
        nubBeams = filter (`Set.notMember` seen) beams
        energized = length $ Set.map fst seen
        newBeams = concatMap (step grid) nubBeams
        newSeen = foldl (flip Set.insert) seen nubBeams

solve :: String -> Int
solve = solve' ((0, 0), R) . parseInput . lines

getEdges :: Int -> Int -> [Beam]
getEdges n m =
  [((0, i), D) | i <- [0 .. m]]
    ++ [((n, i), U) | i <- [0 .. m]]
    ++ [((i, 0), R) | i <- [0 .. n]]
    ++ [((i, m), L) | i <- [0 .. n]]

solve2 :: String -> Int
solve2 s = foldr (max . (`solve'` grid)) 0 (getEdges n m)
  where
    grid = parseInput . lines $ s
    ((_, _), (n, m)) = bounds grid
