import Text.Regex
import Data.List

main = do
    contents <- getContents
    putStr (show(sum(checkGames(readGames(lines contents)))))

data Game = End | GameSet Int Int Int Game deriving (Show)


storeGame :: String -> Game
storeGame [] = End
storeGame x = GameSet red green blue (storeGame ys) where
    xs = last (splitRegex (mkRegex ": ") x)
    z = splitRegex (mkRegex "; ") xs
    y = head z
    ys = intercalate "; " (tail z) -- falta concatenar los elementos de z [String] -> String
    red = readColor $ matchRegex (mkRegex "([0-9]+) red") y
    green = readColor $ matchRegex (mkRegex "([0-9]+) green") y
    blue = readColor $ matchRegex (mkRegex "([0-9]+) blue") y
    --readColor :: Maybe [String] -> Int
    readColor Nothing = 0
    readColor (Just x) = read $ head x

readGames :: [String] -> [Game]
readGames = map storeGame

maxRed = 12
maxGreen = 13
maxBlue = 14

checkGame :: Game -> Bool
checkGame End = True
checkGame (GameSet red green blue xs) = (red <= maxRed && green <= maxGreen && blue <= maxBlue) && checkGame xs

checkGames :: [Game] -> [Int]
checkGames = aux 1 where
    aux _ [] = []
    aux n (x:xs) = if checkGame x then n : aux (n + 1) xs else aux (n + 1) xs
