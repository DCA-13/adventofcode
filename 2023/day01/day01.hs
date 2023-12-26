main = do
    contents <- getContents
    putStr (show(sumValues(lines contents)))
    putStr "\n"
    putStr (show(sumValues2(lines contents)))

firstNumber :: String -> Int
firstNumber [] = 0
firstNumber (x:xs) = if 0 <= d && d <= 9 then d else firstNumber xs where
    d = fromEnum x - 48

lastNumber :: String -> Int
lastNumber = firstNumber.reverse

calibrationValue :: String -> Int
calibrationValue [] = -1
calibrationValue xs = 10 * firstNumber xs + lastNumber xs

sumValues :: [String] -> Int
sumValues = sum.map calibrationValue

firstNumber2 :: String -> Int
firstNumber2 [] = -1
firstNumber2 (x:xs)
    | 0 <= d && d <= 9 = d
    | otherwise = aux (x:xs) where
        d = fromEnum x - 48
        aux ys@(y:zs)
            | take 4 ys == "zero" = 0
            | take 3 ys == "one" = 1
            | take 3 ys == "two" = 2
            | take 5 ys == "three" = 3
            | take 4 ys == "four" = 4
            | take 4 ys == "five" = 5
            | take 3 ys == "six" = 6
            | take 5 ys == "seven" = 7
            | take 5 ys == "eight" = 8
            | take 4 ys == "nine" = 9
            | otherwise = firstNumber2 zs

lastNumber2 :: String -> Int
lastNumber2 = aux (-1) where
    aux :: Int -> String -> Int
    aux lastD [] = lastD
    aux lastD (y:ys)
        | nextD == -1 = lastD
        | otherwise = aux nextD ys where
            nextD = firstNumber2 (y:ys)

calibrationValue2 :: String -> Int
calibrationValue2 [] = -1
calibrationValue2 xs = 10 * firstNumber2 xs + lastNumber2 xs

sumValues2 :: [String] -> Int
sumValues2 = sum.map calibrationValue2

--addDigit :: String -> String
--addDigit [] = []
--addDigit (a:xs@(b:c:_))
--    | [a,b,c] == "one" = '1':a:addDigit xs
--    | [a,b,c] == "two" = '2':a:addDigit xs
--    | [a,b,c] == "six" = '6':a:addDigit xs
--    | otherwise = a:addDigit xs
--addDigit (a:xs@(b:c:d:_))
--    | [a,b,c,d] == "zero" = '0':a:addDigit xs
--    | [a,b,c,d] == "four" = '4':a:addDigit xs
--    | [a,b,c,d] == "five" = '5':a:addDigit xs
--    | [a,b,c,d] == "nine" = '9':a:addDigit xs
--    | otherwise = a:addDigit xs
--addDigit (a:xs@(b:c:d:e:_))
--    | [a,b,c,d,e] == "three" = '3':a:addDigit xs
--    | [a,b,c,d,e] == "seven" = '7':a:addDigit xs
--    | [a,b,c,d,e] == "eight" = '8':a:addDigit xs
--    | otherwise = a:addDigit xs
