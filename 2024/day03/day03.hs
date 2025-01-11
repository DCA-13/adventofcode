{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Data.Char
import Data.Maybe
import System.IO

main = do
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  print . solve $ contents
  -- print . solve2 $ contents
  hClose handle

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ Just . (,x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

spanP' :: (Char -> Bool) -> Parser String
spanP' f =
  Parser $ \input ->
    let (token, rest) = span f input
     in if token /= "" then Just (rest, token) else Nothing

intP :: Parser Int
intP = read <$> spanP' isDigit

spaceP :: Parser String
spaceP = spanP isSpace

instance Monad Parser where
  return = pure
  Parser f >>= g = Parser $ \input ->
    case f input of
      Nothing -> Nothing
      Just (str, a) -> runParser (g a) str

mulP :: Parser Int
mulP = do
  stringP "mul("
  x <- intP
  charP ','
  y <- intP
  charP ')'
  return (x * y)

nonMulP :: Parser Int
nonMulP = Parser $ \case
  "" -> Nothing
  (x : xs) -> Just (xs, 0)

solveP :: Parser Int
solveP = sum <$> many (mulP <|> nonMulP)

solve :: String -> Int
solve input = maybe 0 snd (runParser solveP input)
