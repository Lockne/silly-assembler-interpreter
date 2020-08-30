{-# LANGUAGE FlexibleContexts #-}

module Parser where

import Register
import Text.Parsec
import Data.Char (isAlpha)

parseInc :: Stream s m Char => ParsecT s u m Instruction
parseInc = do
  i <- char 'i'
  n <- char 'n'
  c <- char 'c'
  space
  x <- letter
  return $ Inc x

parseDec :: Stream s m Char => ParsecT s u m Instruction
parseDec = do
  d <- char 'd'
  e <- char 'e'
  c <- char 'c'
  space
  x <- letter
  return $ Dec x

parseJnz :: Stream s m Char => ParsecT s u m Instruction
parseJnz = do
  j <- char 'j'
  n <- char 'n'
  z <- char 'z'
  space
  x <- letter
  space
  y <- minusOrChar
  return $ Jnz x (read y)

parseMov :: Stream s m Char => ParsecT s u m Instruction
parseMov = do
  m <- char 'm'
  o <- char 'o'
  v <- char 'v'
  space
  x <- letter
  space
  y <- minusOrChar
  case isAlpha $ head y of
    True -> return $ MovReg x (head y)
    False -> return $ Mov x (read y)

minusOrChar :: Stream s m Char => ParsecT s u m String
minusOrChar =
   many1 digit <|> ((:) <$> char '-' <*> many1 digit) <|> many1 letter

parseExpr :: Stream s m Char => ParsecT s u m Instruction
parseExpr = parseMov <|> parseInc <|> parseDec <|> parseJnz

readExpr :: String -> Instruction
readExpr input =
  case parse parseExpr "assembler" input of
    Left err -> Err $ "No match: " ++ show err
    Right val -> val
