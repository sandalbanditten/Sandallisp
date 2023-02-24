module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric (readOct, readHex, readFloat)
import Data.Ratio ((%))
import Data.Complex
import Data.Array

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _  -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | String String
  | Bool Bool
  | Character Char
  | Vector (Array Int LispVal)

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  s <- many $ escapedChars <|> noneOf "\\\""
  _ <- char '"'
  return $ String s

parseCharacter :: Parser LispVal
parseCharacter = do
  _ <- try $ string "#\\"
  v <- try (string "newline" <|> string "space") <|> do
    c <- anyChar
    notFollowedBy alphaNum
    return [c]
  return $ Character $ case v of
    "space"   -> ' '
    "newline" -> '\n'
    _         -> head v

escapedChars :: Parser Char
escapedChars = do
  _ <- char '\\'
  c <- oneOf "\\\"nrt"
  return $ case c of
    '\\' -> c
    '"'  -> c
    'n'  -> c
    'r'  -> c
    't'  -> c
    -- oneOf would have failed already
    _    -> undefined

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  -- readFloat return a tuple of the number and the rest of the string
  -- i.g. [(5.0, "")]
  return . Float . fst . head . readFloat $ x ++ "." ++ y

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  _ <- char '/'
  y <- many1 digit
  return $ Ratio $ read x % read y

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal
  _ <- char '+'
  y <- try parseFloat <|> parseDecimal
  _ <- char 'i'
  return $ Complex $ toDouble x :+ toDouble y

toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = fromIntegral n
-- function is not - and should not - be
-- used for anything other than the above
toDouble _ = undefined

parseNumber :: Parser LispVal
parseNumber = parseDecimal
           <|> parsePrefixDecimal
           <|> parseHex
           <|> parseOct
           <|> parseBin

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

parsePrefixDecimal :: Parser LispVal
parsePrefixDecimal = do
  _ <- try $ string "#d"
  d <- many1 digit
  (return . Number . read) d

parseHex :: Parser LispVal
parseHex = do
  _ <- try $ string "#x"
  d <- many1 hexDigit
  return $ Number $ hex2dig d

parseOct :: Parser LispVal
parseOct = do
  _ <- try $ string "#o"
  d <- many1 octDigit
  return $ Number $ oct2dig d

parseBin :: Parser LispVal
parseBin = do
  _ <- try $ string "#b"
  d <- many1 $ oneOf "10"
  return $ Number $ bin2dig d

oct2dig :: String -> Integer
oct2dig = fst . (!! 0) . readOct

hex2dig :: String -> Integer
hex2dig = fst . (!! 0) . readHex

bin2dig :: String -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Integer -> String -> Integer
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = bin2dig' old xs
  where old = 2 * digint + (if x == '0' then 0 else 1)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
  _ <- char ','
  _ <- char '@'
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do arrayVals <- sepBy parseExpr spaces
                 return $ Vector (listArray (0, (length arrayVals - 1)) arrayVals)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         -- these need 'try' because they might start with a #
         -- try will not consume input if it fails
         <|> try parseFloat
         <|> try parseRatio
         <|> try parseComplex
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> parseUnQuoteSplicing
         <|> try (do _ <- string "#("
                     v <- parseVector
                     _ <- char ')'
                     return v)
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

main :: IO ()
main = do
  args <- getArgs
  case args of
    [a] -> putStrLn $ readExpr a
    _   -> print "Provide an expression as first argument"
