import System.IO
import Control.Monad

main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    let cards = map parseCard $ lines contents
        points = map cardPoints cards
        totalPoints = sum points
    putStr $ show totalPoints
    putStr "\n"
    let totalCards = sumCards.addAllCards $ cardsToCards cards
    putStr $ show totalCards
    hClose handle

type Card = ([Int],[Int])

parseCard :: String -> Card
parseCard str = (win,have) where
    numbers = tail.tail $ words str
    win = map read $ takeWhile (/="|") numbers
    have = map read $ reverse $ takeWhile (/="|") $ reverse numbers
    --have = map read $ drop 1 $ dropWhile (/="|") numbers

cardMatches :: Card -> Int
cardMatches ([],_) = 0
cardMatches (x:xs,y) = if x `elem` y then 1 + cardMatches (xs,y) else cardMatches (xs,y)

cardPoints :: Card -> Int
cardPoints card
 | matches == 0 = 0
 | otherwise = 2 ^ (matches - 1) where
    matches = cardMatches card

type Cards = [(Int,Card)]

addCards :: Cards -> Cards
addCards (x@(n,card):xs) = x:newxs where
    matches = cardMatches card
    newxs = aux xs matches
    aux y 0 = y
    aux ((y,c):ys) m = (y + n,c):aux ys (m - 1)

myMap :: ([a] -> [a]) -> [a] -> [a]
myMap _ [] = []
myMap f (x:xs) = head (f (x:xs)) : myMap f (tail(f (x:xs)))

addAllCards :: Cards -> Cards
addAllCards = myMap addCards

sumCards :: Cards -> Int
sumCards x = sum $ map fst x

cardsToCards :: [Card] -> Cards
cardsToCards = map (1,)
