import Data.Char (ord)
import Data.List (unfoldr)
import Data.Map qualified as M
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

hash :: String -> Int
hash = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go ((17 * (acc + ord x)) `mod` 256) xs

split :: (Eq a) => a -> [a] -> [[a]]
split x = unfoldr (fmap coalg) . Just
  where
    coalg t = (f, n)
      where
        (f, dfs) = break (x ==) t
        n = case dfs of
          [] -> Nothing
          (_ : fs) -> Just fs

parseInput :: String -> [String]
parseInput = split ',' . filter (/= '\n')

solve :: String -> Int
solve = sum . map hash . parseInput

type Box = [(String, Int)]

type Boxes = M.Map Int Box

initBoxes :: Boxes
initBoxes = M.fromAscList $ map (,[]) [0 .. 255]

findLabel :: String -> Box -> Bool
findLabel _ [] = False
findLabel label (x : xs) = fst x == label || findLabel label xs

subBox :: (String, Int) -> Box -> Box
subBox _ [] = []
subBox (label, power) (x : xs) =
  if label == fst x
    then (label, power) : xs
    else x : subBox (label, power) xs

updateBox :: (String, Int) -> Box -> Box
updateBox (label, power) box =
  if findLabel label box
    then subBox (label, power) box
    else (label, power) : box

step :: Boxes -> String -> Boxes
step boxes op
  | '-' `elem` op = M.insert boxNum removedBox boxes
  | '=' `elem` op = M.insert boxNum updatedBox boxes
  | otherwise = boxes
  where
    label = takeWhile (\x -> x /= '-' && x /= '=') op
    boxNum = hash label
    currBox = boxes M.! boxNum
    removedBox = filter (\x -> fst x /= label) currBox
    updatedBox = updateBox (label, read [last op]) currBox

focusingPower :: Int -> Box -> Int
focusingPower n box = go 0 0 box
  where
    go acc _ [] = acc
    go acc m ((_, f) : xs) = go (acc + (n + 1) * (l - m) * f) (m + 1) xs
    l = length box

totalPower :: Boxes -> Int
totalPower = M.foldlWithKey (\acc num box -> acc + focusingPower num box) 0

solve2 :: String -> Int
solve2 = totalPower . foldl step initBoxes . parseInput
