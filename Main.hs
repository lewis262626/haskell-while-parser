module Main where

import Text.Parsec

import Data.Char (isAlpha)

parseBool :: Parsec String () Bool
parseBool = do
    value <- (string "true") <|> (string "false")
    return $ chooseVal value
    where chooseVal x = case x of
                            "true" -> True
                            "false" -> False

parseAlphaChar :: Parsec String () Char
parseAlphaChar = satisfy (isAlpha)

identifier :: Parsec String () String
identifier = (:) <$> parseAlphaChar <*> many1 anyChar

main :: IO ()
main = putStrLn "Hello, Haskell!"
