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
    ys = intercalate "; " (tail z)
    red = readColor $ matchRegex (mkRegex "([0-9]+) red") y
    green = readColor $ matchRegex (mkRegex "([0-9]+) green") y
    blue = readColor $ matchRegex (mkRegex "([0-9]+) blue") y
    readColor Nothing = 0
    readColor (Just x) = read $ head x

idGame :: String -> Int
idGame [] = 0
idGame x = readId $ matchRegex (mkRegex "Game ([0-9]+)") x where
    readId Nothing = 0
    readId (Just x) = read $ head x

readGames :: [String] -> [(Int,Game)]
readGames = map (\x -> (idGame x, storeGame x))

maxRed = 12
maxGreen = 13
maxBlue = 14

checkGame :: Game -> Bool
checkGame End = True
checkGame (GameSet red green blue xs) = (red <= maxRed && green <= maxGreen && blue <= maxBlue) && checkGame xs

checkGames :: [(Int,Game)] -> [Int]
checkGames = map fst . filter (checkGame . snd)
