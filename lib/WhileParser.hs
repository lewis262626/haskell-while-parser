module WhileParser where

import Text.Parsec

import Data.Char (isAlpha)

type Variable = String

data Value = 
    IntValue Int
    | BoolVal Bool
    deriving (Show, Eq)

parseBool :: Parsec String () Value
parseBool = do
    value <- (string "true" <* eof) <|> (string "false" <* eof)
    return $ chooseVal value
    where chooseVal x = case x of
                            "true" -> BoolVal True
                            "false" -> BoolVal False

parseConstant :: Parsec String () Value
parseConstant = do
    num <- many1 $ oneOf "0123456789"
    let readNum = read num :: Int
    return $ IntValue readNum

valueP :: Parsec String () Value
valueP = parseBool <|> parseConstant

parseAlphaChar :: Parsec String () Char
parseAlphaChar = satisfy isAlpha

identifier :: Parsec String () String
identifier = many1 parseAlphaChar

skipStatement :: Parsec String () String
skipStatement = string "skip ;" <* eof