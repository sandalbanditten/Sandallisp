module Main where

import           Control.Monad.Except
import           Numeric                       (readHex, readOct)
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)
import           Data.Functor ((<&>))
import           System.IO
import           Data.IORef
import           Data.Maybe (isJust, isNothing)

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
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func { params  :: [String]
         , vararg  :: Maybe String
         , body    :: [LispVal]
         , closure :: Env
         }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle
  -- | Character Char
  -- | Vector (Array Int LispVal)

type Env = IORef [(String, IORef LispVal)]

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Port _)          = "<IO port"
showVal (IOFunc _)        = "<IO primitive"
showVal (Func { params  = args
              , vararg  = varargs
              , body    = _
              , closure = _ }) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
        Nothing  -> ""
        Just arg -> " . " ++ arg) ++ ") ... )"
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
showError (Default str)                 = "Defaulting error " ++ str

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
  let atom = first:rest
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

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _)
  = return val
eval _ val@(Number _)
  = return val
eval _ val@(Bool _)
  = return val
eval env (Atom ident)
  = getVar env ident
eval _ (List [Atom "quote", val])
  = return val
eval env (List [Atom "if", p, a, b])
  -- if p then a else b
  -- will evaluate #f as False and *everything* else as True
  = do result <- eval env p
       case result of
            Bool False -> eval env b
            _          -> eval env a
eval env (List [Atom "set!", Atom var, form])
  = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form])
  = eval env form >>= defVar env var
eval env (List (Atom "define" : List (Atom var : params') : body'))
  = makeNormalFunc env params' body' >>= defVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body'))
  = makeVarArgs varargs env params' body' >>= defVar env var
eval env (List (Atom "lambda" : List params' : body'))
  = makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body'))
  = makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body'))
  = makeVarArgs varargs env [] body'
eval env (List (function:args))
  = do func <- eval env function
       argVals <- mapM (eval env) args
       apply func argVals
eval _ badForm
  = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params' body' = return $ Func (map showVal params') varargs body' env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params' varargs' body' closure') args =
  if num params' /= num args && isNothing varargs'
    then throwError $ NumArgs (num params') args
    else liftIO (bindVars closure' $ zip params' args) >>= bindVarArgs varargs' >>= evalBody
  where
    remainingArgs       = drop (length params') args
    num                 = toInteger . length
    evalBody env        = last <$> mapM (eval env) body'
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing      -> return env
apply _ _ = undefined -- it is guaranteed

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

-- a combined monad for LispErrors and IO
type IOThrowsError = ExceptT LispError IO

-- a lifting function from our old monad to the combined one
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows = (<&> extractValue) . runExceptT . trapError

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . lookup var)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . flip writeIORef val)
    (lookup var env)
  return val

defVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var val >> return val
    else liftIO $ do
      valRef <- newIORef val
      env <- readIORef envRef
      writeIORef envRef ((var, valRef):env)
      return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef
                        >>= extendEnv bindings
                        >>= newIORef
  where
    extendEnv binds env   = fmap (++ env) (mapM addBinding binds)
    addBinding (var, val) = do ref <- newIORef val
                               return (var, ref)

flushStr :: String -> IO ()
flushStr = (>> hFlush stdout) . putStr

readPrompt :: String -> IO String
readPrompt = (>> getLine) . flushStr

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- loops until the predicate V  is true
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

nullEnv :: IO Env
nullEnv = newIORef []

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Scheme λ: ") . evalAndPrint

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ head args
    _ -> putStrLn "No args to run REPL, 1 arg to evaluate it"
