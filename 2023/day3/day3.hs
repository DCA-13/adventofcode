import Data.List
import Data.Char
import System.IO
import Control.Monad

main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    let total = sum.solveLines $ lines contents
    putStr $ show total
    hClose handle

findSymbols :: String -> [Int]
findSymbols = findIndices isSymbol where
    isSymbol x
        | isDigit x || x == '.' = False
        | otherwise = True

parseLine :: String -> [(Int,Int,Int)]
parseLine str = aux 0 str where
    aux _ [] = []
    aux acc str | (x,xs) <- break isDigit str,
                  (y@(_:_),ys) <- span isDigit xs,
                  start <- acc + length x,
                  end <- start + length y - 1
                  = (read y, start, end) : aux (end + 1) ys
    aux _ _ = []

windows :: [String] -> [[String]]
windows (x:y:z:xs) = [x,y,z] : windows (y:z:xs)
windows _ = []

solveLine :: String -> [Int] -> [Int]
solveLine str s = aux where
    l = parseLine str
    adjacent sy (_,start,end) = sy >= start - 1 && sy <= end + 1
    aux = map (\(n,_,_) -> n) $ filter (\n -> any (\f -> f n) fs) l
    fs = map adjacent s

solveEdge [x,y] = solveLine x $ concatMap findSymbols [x,y]

solveLines :: [String] -> [Int]
solveLines x = firstLine ++ middleLines ++ lastLine where
    firstLine = solveEdge $ take 2 x
    middleLines = concat $ zipWith solveLine (tail $ init x) (map (concatMap findSymbols) (windows x))
    lastLine = solveEdge $ take 2 $ reverse x

test = "467..114..\n\
        \...*......\n\
        \..35..633.\n\
        \......#...\n\
        \617*......\n\
        \.....+.58.\n\
        \..592.....\n\
        \......755.\n\
        \...$.*....\n\
        \.664.598.."
