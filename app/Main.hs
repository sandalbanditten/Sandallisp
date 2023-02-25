{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Control.Monad.Except
import           Numeric                       (readHex, readOct)
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Data.Functor ((<&>))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right v  -> return v

spaces :: Parser ()
spaces = skipMany1 space

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  -- | Float Double
  -- | Ratio Rational
  -- | Complex (Complex Double)
  | String String
  | Bool Bool
  -- | Character Char
  -- | Vector (Array Int LispVal)

-- TODO: How to print the remaining?
showVal :: LispVal -> String
showVal (Atom name)       = name
showVal (List xs)         = "(" ++ unwordsList xs ++ ")"
showVal (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ showVal x ++ ")"
showVal (Number num)      = show num
-- showVal (Float _num)      = undefined
-- showVal (Ratio _num)      = undefined
-- showVal (Complex _num)    = undefined
showVal (String str)      = "\"" ++ str ++ "\""
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
-- showVal (Character c)     = "\\#" ++ [c]
-- showVal (Vector _vec)     = undefined

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar msg var)          = msg ++ ": " ++ var
showError (BadSpecialForm msg form)     = msg ++ ": " ++ show form
showError (NotFunction msg func)        = msg ++ ": " ++ show func
showError (NumArgs expected found)      = "Expecteed "
                                        ++ show expected
                                        ++ " args; found values "
                                        ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected "
                                        ++ expected
                                        ++ ", found "
                                        ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
-- Should be called after trapError
-- to guarantee a Right and no Left
extractValue _           = undefined


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

{-
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
-}

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

{-
parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  _ <- char '.'
  y <- many1 digit
  -- readFloat return a tuple of the number and the rest of the string
  -- i.g. [(5.0, "")]
  return . Float . fst . head . readFloat $ x ++ "." ++ y
-}

{-
parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  _ <- char '/'
  y <- many1 digit
  return $ Ratio $ read x % read y
-}

{-
parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal
  _ <- char '+'
  y <- try parseFloat <|> parseDecimal
  _ <- char 'i'
  return $ Complex $ toDouble x :+ toDouble y
-}

-- toDouble :: LispVal -> Double
-- toDouble (Float f)  = realToFrac f
-- toDouble (Number n) = fromIntegral n
-- function is not - and should not - be
-- used for anything other than the above
-- toDouble _ = undefined

parseNumber :: Parser LispVal
parseNumber = parseDecimal
           <|> parsePrefixDecimal
           <|> parseHex
           <|> parseOct
           <|> parseBin

parseDecimal :: Parser LispVal
parseDecimal = many1 digit <&> (Number . read)

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
oct2dig = fst . head . readOct

hex2dig :: String -> Integer
hex2dig = fst . head . readHex

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

{-
parseVector :: Parser LispVal
parseVector = do arrayVals <- sepBy parseExpr spaces
                 return $ Vector (listArray (0, length arrayVals - 1) arrayVals)
-}

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         -- these need 'try' because they might start with a #
         -- try will not consume input if it fails
         -- <|> try parseFloat
         -- <|> try parseRatio
         -- <|> try parseComplex
         <|> try parseNumber
         <|> try parseBool
         -- <|> try parseCharacter
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> parseUnQuoteSplicing
         {-
         <|> try (do _ <- string "#("
                     v <- parseVector
                     _ <- char ')'
                     return v)
          -}
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

eval :: LispVal -> ThrowsError LispVal
eval val@(String _)              = return val
eval val@(Number _)              = return val
eval val@(Bool _)                = return val
eval (List [Atom "quote", val])  = return val
eval (List [Atom "if", p, a, b]) = -- if p then a else b
  -- will evaluate #f as False and *everything* else as True
  do result <- eval p
     case result of
          Bool False -> eval b
          _          -> eval a
eval (List (Atom func : args))   = mapM eval args >>= apply func
eval badForm                     = throwError
                                 $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args)
  (lookup func primitives)

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgList           = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []]         = return     $ List [x]
cons [x, List xs]         = return     $ List (x:xs)
cons [x, DottedList xs y] = return     $ DottedList (x:xs) y
cons [x, y]               = return     $ DottedList [x] y
cons badArgList           = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y]                   = return $ Bool $ x == y
eqv [Number x, Number y]               = return $ Bool $ x == y
eqv [String x, String y]               = return $ Bool $ x == y
eqv [Atom x, Atom y]                   = return $ Bool $ x == y
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List xs, List ys]                 = return
                                       $ Bool
                                       $ length xs == length ys
                                       && all eqvPair (zip xs ys)
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ NumArgs 2 badArgList

eqvPair :: (LispVal, LispVal) -> Bool
eqvPair (x, y) =
  case eqv [x, y] of
    Right (Bool v) -> v
    -- these will not happen, as eqv only error if it gets not 2 args
    Left _         -> False
    _              -> undefined

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",         numericBinop  (+) ),
              ("-",         numericBinop  (-) ),
              ("*",         numericBinop  (*) ),
              ("/",         numericBinop  div ),
              ("mod",       numericBinop  mod ),
              ("quotient",  numericBinop  quot),
              ("remainder", numericBinop  rem ),
              ("=",         numBoolBinop  (==)),
              ("<",         numBoolBinop  (<) ),
              (">",         numBoolBinop  (>) ),
              ("/=",        numBoolBinop  (/=)),
              (">=",        numBoolBinop  (>=)),
              ("<=",        numBoolBinop  (<=)),
              ("&&",        boolBoolBinop (&&)),
              ("||",        boolBoolBinop (||)),
              ("string=?",  strBoolBinop  (==)),
              ("string<?",  strBoolBinop  (<) ),
              ("string>?",  strBoolBinop  (>) ),
              ("string<=?", strBoolBinop  (<=)),
              ("string>=?", strBoolBinop  (>=)),
              ("car",       car               ),
              ("cdr",       cdr               ),
              ("cons",      cons              ),
              ("eq?",       eqv               ),
              ("eqv?",      eqv               ),
              ("equal?",    equal             )]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ []            = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op args         = mapM unpackNum args <&> (Number . foldl1 op)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    -- list indexing is has been bounds checked by if-predicate
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return $ Bool $ left `op` right


numBoolBinop ::  (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop ::  (String  -> String  -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop :: (Bool    -> Bool    -> Bool) -> [LispVal] -> ThrowsError LispVal

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String str) = return str
unpackStr (Number num) = return $ show num
unpackStr (Bool bool)  = return $ show bool
unpackStr notString    = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool bool) = return bool
unpackBool notBool     = throwError $ TypeMismatch "boolean" notBool

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals a b (AnyUnpacker unpacker) =
  do ua <- unpacker a
     ub <- unpacker b
     return $ ua == ub
     `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [a, b]      = do
  primitiveEquals <- or <$> mapM (unpackEquals a b)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [a, b]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

flushStr :: String -> IO ()
flushStr = (>> hFlush stdout) . putStr

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
