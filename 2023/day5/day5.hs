import Control.Monad
import Data.Maybe
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  putStr . show . solve $ contents
  putStr "\n"
  putStr . show . solve2 $ contents
  hClose handle

seedList :: String -> [Int]
seedList = map read . tail . words . head . lines

splitList :: (Eq a) => a -> [a] -> [[a]]
splitList _ [] = []
splitList x xs = y : ys
  where
    y = takeWhile (/= x) xs
    n = length y
    ys = splitList x (drop (n + 1) xs)

data Map = Map
  { dest :: Int,
    src :: Int,
    range :: Int
  }
  deriving (Show)

splitMaps :: String -> [[String]]
splitMaps = map tail . tail . splitList "" . lines

parseMap :: String -> Map
parseMap str = Map {dest = d, src = s, range = r}
  where
    [d, s, r] = map read . words $ str

parseMaps :: [String] -> [Map]
parseMaps = map parseMap

mapNumber :: Map -> Int -> Maybe Int
mapNumber m n
  | src m <= n && n < src m + range m = Just (dest m + n - src m)
  | otherwise = Nothing

mapsNumber :: [Map] -> Int -> Int
mapsNumber maps n
  | all ((== Nothing) . (`mapNumber` n)) maps = n
  | otherwise = fromJust . head . filter (/= Nothing) . map (`mapNumber` n) $ maps

getLocation :: [[Map]] -> Int -> Int
getLocation maps n = foldr mapsNumber n $ reverse maps

solve :: String -> Int
solve str = minimum . map (getLocation maps) $ seeds
  where
    maps = map parseMaps . splitMaps $ str
    seeds = seedList str

-- seedRanges :: String -> [Int]
-- seedRanges = aux . seedList
--  where
--    aux [] = []
--    aux nums = [x .. x + r - 1] ++ (aux . drop 2 $ nums)
--      where
--        [x, r] = take 2 nums
--
-- solve2 :: String -> Int
-- solve2 str = minimum . map (\x -> foldr mapsNumber x $ reverse maps) $ seeds
--  where
--    maps = map parseMaps . splitMaps $ str
--    seeds = seedRanges str

validSeed :: [Int] -> Int -> Bool
validSeed [] _ = False
validSeed (x:y:xs) n
  | 0 <= n - x && n - x < y = True
  | otherwise = validSeed xs n

reverseMap :: Map -> Map
reverseMap Map {dest = d, src = s, range = r} = Map {dest = s, src = d, range = r}

reverseMaps :: [Map] -> Int -> Int
reverseMaps maps = mapsNumber (map reverseMap maps)

getSeed :: [[Map]] -> Int -> Int
getSeed maps n = foldr reverseMaps n maps
-- foldr mapsNumber n reverseMaps

solve2 :: String -> Int
solve2 str = head . filter (validSeed seeds . getSeed maps) $ [1..]
  where
    maps = map parseMaps . splitMaps $ str
    seeds = map read . tail . words . head . lines $ str
