import Control.Monad
import Data.List
import Data.Map qualified as M
import Data.Maybe
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

data Node = Node {left :: String, right :: String} deriving (Show)

type NodeMap = M.Map String Node

stringToNode :: String -> (String, Node)
stringToNode str = (nodeId, Node {left = leftNode, right = rightNode})
  where
    [nodeId, _, leftNode', rightNode'] = words str
    leftNode = tail . init $ leftNode'
    rightNode = init rightNode'

nodeMap :: [String] -> NodeMap
nodeMap = M.fromList . map stringToNode

step :: Char -> Maybe String -> NodeMap -> Maybe String
step 'L' str nodes = left <$> (str >>= flip M.lookup nodes)
step 'R' str nodes = right <$> (str >>= flip M.lookup nodes)
step _ _ _ = Nothing

steps :: String -> Maybe String -> NodeMap -> [Maybe String]
steps (dir : dirs) start nodes = newStart : steps dirs newStart nodes
  where
    newStart = step dir start nodes

solve :: String -> Int
solve str = 1 + (length . takeWhile (\x -> isJust x && (x /= Just "ZZZ")) $ steps (cycle dirs) (Just "AAA") nodes)
  where
    dirs : _ : xs = lines str
    nodes = nodeMap xs

-- solve2' :: String -> NodeMap -> String -> [Int]
-- solve2' dirs nodes key = map (+1) . findIndices (\x -> Just 'Z' == (last <$> x)) $ steps (cycle dirs) (Just key) nodes
--
-- elem' :: Ord a => a -> [a] -> Bool
-- elem' _ [] = False
-- elem' a (x:xs)
--   | a == x = True
--   | a < x = False
--   | a > x = elem' a xs
--
-- solve2 :: String -> Int
-- solve2 str = head . filter (\n -> all (\xs -> n `elem'` xs) zIndices) $ [1 ..]
--   where
--     dirs : _ : xs = lines str
--     nodes = nodeMap xs
--     keys = M.keys nodes
--     aKeys = filter (\s -> last s == 'A') keys
--     zIndices = map (solve2' dirs nodes) aKeys

solve2' :: String -> NodeMap -> String -> Int
solve2' dirs nodes key = 1 + (length . takeWhile (\x -> isJust x && ((last <$> x) /= Just 'Z')) $ steps (cycle dirs) (Just key) nodes)

solve2 :: String -> Int
solve2 str = foldl1 lcm aSteps
  where
    dirs : _ : xs = lines str
    nodes = nodeMap xs
    keys = M.keys nodes
    aKeys = filter (\s -> last s == 'A') keys
    aSteps = map (solve2' dirs nodes) aKeys
