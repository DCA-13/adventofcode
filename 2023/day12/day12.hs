import Data.List (intercalate, partition)

main = do
  input <- readFile "input"
  print . solve $ input
  print . solve2 $ input

data Row = Row {past :: String, record :: String, groups :: [Int], counter :: Int, currGroup :: Int, num :: Int}

instance Show Row where
  show (Row p r g c cg n) = unwords ["Row", show $ reverse p, show r, show g, 'c' : show c, 'g' : show cg, 'n' : show n] ++ "\n"

test = "?###???????? 3,2,1"

unfoldRow :: Row -> Row
unfoldRow (Row p r gs c g n) = Row p (intercalate "?" . replicate 5 $ r) (concat . replicate 5 $ gs) c g n

nextSpring :: Row -> [Row]
nextSpring (Row _ [] gs c g n) = error "ran out of input"
nextSpring row@(Row p (r : rs) gs c g n) = case r of
  '?' -> concatMap nextSpring [row {record = '.' : rs}, row {record = '#' : rs}]
  '.'
    | c == 0 -> [Row (r : p) rs gs 0 g n]
    | c == gs !! g -> [Row (r : p) rs gs 0 (g + 1) n]
    | otherwise -> []
  '#'
    | g == length gs -> []
    | c == gs !! g -> []
    | otherwise -> [Row (r : p) rs gs (c + 1) g n]
  e -> error $ "unexpected " ++ [e] ++ ", expected .#?"

combineRows :: [Row] -> [Row]
combineRows [] = []
combineRows (r : rs) = newRow : combineRows rest
  where
    identical r1 r2 = counter r1 == 0 && counter r2 == 0 && currGroup r1 == currGroup r2
    (idRows, rest) = partition (identical r) rs
    newRow = r {num = num r + sum (map num idRows)}

step :: [Row] -> [Row]
step = combineRows . concatMap nextSpring

validateRow :: Row -> Bool
validateRow (Row p r gs c g n) = g == length gs || g == length gs - 1 && c == last gs

solveRow :: Row -> Int
solveRow row = sum $ map num valid
  where
    total = iterate step [row] !! (length . record $ row)
    valid = filter validateRow total

parseRow :: String -> Row
parseRow str = Row "" r (read $ "[" ++ g ++ "]") 0 0 1
  where
    [r, g] = words str

solve :: String -> Int
solve input = sum . map (solveRow . parseRow) $ lines input

solve2 :: String -> Int
solve2 input = sum . map (solveRow . unfoldRow . parseRow) $ lines input
