import Control.Monad
import Data.Map qualified as M
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

type Tiles = M.Map (Int, Int) Char

type Distances = M.Map (Int, Int) Int

n2 = map (\n -> map (n,) [1 ..]) [1 ..]

parseInput :: String -> Tiles
parseInput = M.fromAscList . concat . zipWith zip n2 . lines

findStart :: Tiles -> (Int, Int)
findStart tiles = head . map fst $ filter (\x -> snd x == 'S') (M.assocs tiles)

initDists :: Tiles -> Distances
initDists tiles = M.singleton (findStart tiles) 0

adjacentPipes :: (Int, Int) -> Char -> [(Int, Int)]
adjacentPipes (a, b) p
  | p == '|' = [(a - 1, b), (a + 1, b)]
  | p == 'J' = [(a - 1, b), (a, b - 1)]
  | p == 'L' || p == 'S' = [(a - 1, b), (a, b + 1)]
  | p == '7' = [(a + 1, b), (a, b - 1)]
  | p == 'F' = [(a + 1, b), (a, b + 1)]
  | p == '-' = [(a, b - 1), (a, b + 1)]

insertMaybeWith :: (Ord k) => (a -> a -> a) -> k -> Maybe a -> M.Map k a -> M.Map k a
insertMaybeWith _ _ Nothing map = map
insertMaybeWith f k (Just x) map = M.insertWith f k x map

insertNewDistances :: (Int, Int) -> [(Int, Int)] -> Distances -> Distances
insertNewDistances _ [] dists = dists
insertNewDistances p (x : xs) dists = insertMaybeWith min x ((+ 1) <$> M.lookup p dists) (insertNewDistances p xs dists)

distsIteration :: Tiles -> Distances -> Distances -> Distances
distsIteration tiles dists acc = M.foldrWithKey insertNewDistances dists . M.mapWithKey adjacentPipes $ M.mapMaybeWithKey (\k _ -> M.lookup k tiles) (M.difference dists acc)

fixAcc :: (Eq a) => (a -> a -> a) -> a -> a -> a
fixAcc f x acc = if x == f x acc then x else fixAcc f (f x acc) x

fullLoop :: Tiles -> Distances -> Distances
fullLoop tiles dists = fixAcc (distsIteration tiles) dists M.empty

solve :: String -> Int
solve input = maximum $ fullLoop tiles dists
  where
    tiles = parseInput input
    dists = initDists tiles

isInLoop :: Tiles -> (Int, Int) -> Bool
isInLoop loop (x,y) = odd . M.size . M.filterWithKey (\(a,b) c -> x - a == y - b && a < x && c /= '7' && c /= 'L' && c /= 'S') $ loop

loopArea :: Tiles -> Tiles -> Int
loopArea tiles loop = M.size . M.filterWithKey (\k _ -> isInLoop loop k) $ tiles

solve2 :: String -> Int
solve2 input = loopArea nonLoop loop
  where
    tiles = parseInput input
    dists = initDists tiles
    loop = M.intersection tiles $ fullLoop tiles dists
    nonLoop = M.difference tiles loop
