import Control.Monad
import Data.Char
import Data.List
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let total = sum . solveLines $ lines contents
  let total2 = sum . gearRatios . gearParts $ contents
  print total
  print total2
  hClose handle

findSymbols :: String -> [Int]
findSymbols = findIndices isSymbol
  where
    isSymbol x = not (isDigit x || x == '.')

parseLine :: String -> [(Int, Int, Int)]
parseLine str = aux 0 str
  where
    aux _ [] = []
    aux acc str
      | (x, xs) <- break isDigit str,
        (y@(_ : _), ys) <- span isDigit xs,
        start <- acc + length x,
        end <- start + length y - 1 =
          (read y, start, end) : aux (end + 1) ys
    aux _ _ = []

windows :: [a] -> [[a]]
windows (x : y : z : xs) = [x, y, z] : windows (y : z : xs)
windows _ = []

solveLine :: String -> [Int] -> [Int]
solveLine str s = aux
  where
    l = parseLine str
    adjacent sy (_, start, end) = sy >= start - 1 && sy <= end + 1
    aux = map (\(n, _, _) -> n) $ filter (\n -> any (\f -> f n) fs) l
    fs = map adjacent s

solveEdge [x, y] = solveLine x $ concatMap findSymbols [x, y]

solveLines :: [String] -> [Int]
solveLines x = firstLine ++ middleLines ++ lastLine
  where
    firstLine = solveEdge $ take 2 x
    middleLines = concat $ zipWith solveLine (tail $ init x) (map (concatMap findSymbols) (windows x))
    lastLine = solveEdge $ take 2 $ reverse x

test =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598.."

findGears :: (Int, String) -> [(Int, Int)]
findGears (r, str) = map (r,) (elemIndices '*' str)

solveLine2' :: String -> [(Int, Int)] -> [(Int, Int, Int)]
solveLine2' str gears = lineGears
  where
    line = parseLine str
    adjacent (r, c) (n, start, end)
      | c >= start - 1 && c <= end + 1 = [(n, r, c)]
      | otherwise = []
    adjacentToGears = map adjacent gears
    surroundingGears a = concatMap ($ a) adjacentToGears
    lineGears = concatMap surroundingGears line

solveLine2 :: [(Int, String)] -> [(Int, Int, Int)]
solveLine2 x = solveLine2' (snd $ x !! 1) (concatMap findGears x)

gearParts :: String -> [(Int, Int, Int)]
gearParts str = firstLine ++ middleLines ++ lastLine
  where
    lns = lines str
    numLines = zip [0 ..] lns
    firstLine = solveLine2' (head lns) (concatMap findGears $ take 2 numLines)
    middleLines = concatMap solveLine2 (windows numLines)
    lastLine = solveLine2' (last lns) (concatMap findGears $ take 2 $ reverse numLines)

getGear :: (Int, Int, Int) -> (Int, Int)
getGear (_, b, c) = (b, c)

getNum :: (Int, Int, Int) -> Int
getNum (a, _, _) = a

gearRatios :: [(Int, Int, Int)] -> [Int]
gearRatios [] = []
gearRatios (p : ps)
  | null parts = gearRatios ps
  | otherwise = getNum p * getNum (head parts) : gearRatios ps
  where
    parts = filter (\x -> getGear p == getGear x) ps
