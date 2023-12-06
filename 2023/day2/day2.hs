import Text.Regex

--main = do
--    contents <- getContents

data Game = End | GameSet Int Int Int Game deriving (Show)

storeGame :: String -> Game
storeGame [] = End
storeGame xs = 
