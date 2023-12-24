import Control.Monad
import Data.Char
import Data.List
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  print . solve2 $ contents
  hClose handle

data HandType = High | OnePair | TwoPair | Three | Full | Four | Five deriving (Eq, Ord, Show)

data Hand = Hand { cards :: String, bid :: Int } deriving Show

count :: Eq a => [a] -> [Int]
count xs = sort . map (\x -> length . filter (== x) $ xs) . nub $ xs

getHand :: String -> HandType
getHand str
  | count str == [5] = Five
  | count str == [1, 4] = Four
  | count str == [2, 3] = Full
  | count str == [1, 1, 3] = Three
  | count str == [1, 2, 2] = TwoPair
  | count str == [1, 1, 1, 2] = OnePair
  | otherwise = High

highestCard :: Char -> Char -> Ordering
highestCard x y
  | x == y = EQ
  | isDigit x && isDigit y = if x < y then LT else GT
  | isDigit x = LT | isDigit y = GT
  | x == 'A' = GT  | y == 'A' = LT
  | x == 'K' = GT  | y == 'K' = LT
  | x == 'Q' = GT  | y == 'Q' = LT
  | x == 'J' = GT  | y == 'J' = LT
  | otherwise = error "not a card"

compareHighCard :: String -> String -> Ordering
compareHighCard [] _ = EQ
compareHighCard _ [] = EQ
compareHighCard (x : xs) (y : ys)
  | x == y = compareHighCard xs ys
  | otherwise = highestCard x y

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2
  | getHand c1 < getHand c2 = LT
  | getHand c1 > getHand c2 = GT
  | otherwise = compareHighCard c1 c2
  where
    c1 = cards h1
    c2 = cards h2

rankHands :: [Hand] -> [Hand]
rankHands = sortBy compareHands

solve :: String -> Int
solve str = sum $ zipWith (\n (Hand {cards = _, bid = b}) -> n * b) [1 ..] hands
  where
    hands = rankHands . map ((\[c, b] -> Hand {cards = c, bid = read b}) . words) . lines $ str

sub :: Eq a => a -> a -> [a] -> [a]
sub n o = map (\x -> if x == o then n else x)

getHand2 :: String -> HandType
getHand2 str = maximum . map (\c -> getHand $ sub c 'J' str) $ nub str

highestCard2 :: Char -> Char -> Ordering
highestCard2 x y
  | x == y = EQ
  | x == 'J' = LT  | y == 'J' = GT
  | isDigit x && isDigit y = if x < y then LT else GT
  | isDigit x = LT | isDigit y = GT
  | x == 'A' = GT  | y == 'A' = LT
  | x == 'K' = GT  | y == 'K' = LT
  | x == 'Q' = GT  | y == 'Q' = LT
  | otherwise = error "not a card"

compareHighCard2 :: String -> String -> Ordering
compareHighCard2 [] _ = EQ
compareHighCard2 _ [] = EQ
compareHighCard2 (x : xs) (y : ys)
  | x == y = compareHighCard2 xs ys
  | otherwise = highestCard2 x y

compareHands2 :: Hand -> Hand -> Ordering
compareHands2 h1 h2
  | getHand2 c1 < getHand2 c2 = LT
  | getHand2 c1 > getHand2 c2 = GT
  | otherwise = compareHighCard2 c1 c2
  where
    c1 = cards h1
    c2 = cards h2

rankHands2 :: [Hand] -> [Hand]
rankHands2 = sortBy compareHands2

solve2 :: String -> Int
solve2 str = sum $ zipWith (\n (Hand {cards = _, bid = b}) -> n * b) [1 ..] hands
  where
    hands = rankHands2 . map ((\[c, b] -> Hand {cards = c, bid = read b}) . words) . lines $ str
