module Lambda.LamParser where

import Control.Applicative
import Data.Char
import Data.Set (Set, member, fromList)
import System.FilePath (isValid)
import Distribution.Compat.Prelude (readMaybe)

data Expr =
    EVar String |
    EAbs String TypeExpr Expr |
    EApp Expr Expr |
    ELet String Expr Expr |
    EIf Expr Expr Expr |
    EBool String |
    EUnit |
    ETuple [Expr] |
    ERecord [(String, Expr)] |
    EPair Expr Expr |
    EProj Expr Int |
    EField Expr String |
    EInt Int
    deriving Show

data TypeExpr
  = TeBool
  | TeInt
  | TeTop
  | TeUnit
  | TePair TypeExpr TypeExpr
  | TeTuple [TypeExpr]
  | TeRecord [(String, TypeExpr)]
  | TeAbs TypeExpr TypeExpr
  | TeUnknown
  deriving Show


-- Parser def

-- Parser of a is a thing that can read a source string,
-- then return Nothing or rest source and a parsed thing.
newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
}

-- For each Parser<a> we can inject a function a->b and get Parser<b>
instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', a) <- p input
            Just (input', f a)

-- For each two parsers (a and b), we can apply them sequentially and
-- give back result of (a b) application.
-- For each single parser, we can just apply it to code.
instance Applicative Parser where
    pure a =
        Parser $ \input ->
            Just (input, a)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', a) <- p1 input
            (input'', b) <- p2 input'
            Just (input'', a b)

-- For each two parsers we can apply first one and then second,
-- if first has failed to parse.
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
      Parser $ \input -> p1 input <|> p2 input


instance Monad Parser where
    return = pure
    (Parser p) >>= somethingAfter = Parser $ \input -> do
        (input', a) <- p input
        let nextParser = somethingAfter a
        runParser nextParser input'

-- Parser impl

pChar :: Char -> Parser Char
pChar char = Parser f
    where
        f (first : rest)
            | first == char = Just (rest, char)
            | otherwise = Nothing
        f [] = Nothing

pString :: String -> Parser String
pString = traverse pChar

-- Eat all heading characters applicable to predicate
pSpanOf predicate = Parser $ \input ->
    let (token, rest) = span predicate input
    in Just (rest, token)

pNonEmptySpanOf predicate = Parser $ \input -> do
    let (token, rest) = span predicate input
    if rest == input then Nothing else Just (rest, token)

notNull (Parser someParser) = Parser $ \input -> do
    (input', something) <- someParser input
    if null something then Nothing else Just (input', something)

-- Eat all whitespaces
ws = pSpanOf isSpace

-- Eat trailing whitespace
token :: Parser a -> Parser a
token p = p <* ws

word :: String -> Parser String
word s = token (pString s)

-- Parses one or more occurrences of p, separated by sep
pSeparated1 :: Parser a -> Parser s -> Parser [a]
pSeparated1 p sep = (:) <$> p <*> many (sep *> p)

-- Parses zero or more occurrences of p, separated by sep
pSeparated :: Parser a -> Parser s -> Parser [a]
pSeparated p sep = pSeparated1 p sep <|> pure []


-- Grammar impl

isReservedKeyword kw = member kw $ fromList ["if", "let", "then", "else", "True", "Unit", "False"]

isValidIdChar '\'' = True
isValidIdChar '_' = True
isValidIdChar someChar = isAlphaNum someChar

pIdentifierNotKeyword = Parser $ \input -> do
  let (token, rest) = span isValidIdChar input
  if rest == input || isReservedKeyword token then Nothing else Just (rest, token)

pPrimary =
    ((\str -> EInt $ read str) <$> pNonEmptySpanOf isNumber) <|>
    (EVar <$> pIdentifierNotKeyword) <|>
    (EBool <$> word "True") <|>
    (EBool <$> word "False") <|>
    ((\_ -> EUnit) <$> word "Unit") <|>
    pTuple <|>
    pRecord <|>
    pPairOrExpr

pPairOrExpr = do
  word "("
  item1 <- pExpr
  word ")"
  return item1
  --ws
  --(EPair item1 <$> (word "," *> pExpr)) <|> return item1
  -- (item1, item2) -- pair or just (expr) -- expr

pTuple = do
  word "["
  items <- pSeparated pExpr (ws <* word ",")
  ws
  word "]"
  return $ ETuple items

pRecord = do
  word "{"
  fields <- pSeparated pRecordField (ws <* word ",")
  ws
  word "}"
  return $ ERecord fields
  where
    pRecordField = do
      var <- pIdentifierNotKeyword
      ws
      word "="
      expr <- pExpr
      ws
      return (var, expr)

pAbs = do
    word "\\" <|> word "λ"
    boundVar <- pIdentifierNotKeyword
    ws
    word ":"
    ty <- pTypeExpr
    ws
    word "."
    body <- pExpr
    return $ EAbs boundVar ty body

pApp = do
    function <- (pLet <|> pAbs <|> pPrimary)
    ws -- Parse all consequent terms
    arguments <- many (ws *> (pLet <|> pAbs <|> pPrimary) <* ws)
    return $ foldl EApp function arguments -- Fold 'em left-associatively

pLet = do
    word "let"
    boundVar <- pIdentifierNotKeyword
    ws
    word "="
    boundExpr <- pExpr
    ws
    word "," <|> word "in"
    following <- pExpr
    return $ ELet boundVar boundExpr following

pIf = do
  word "if"
  condition <- pExpr
  ws
  word "then"
  thenBranch <- pExpr
  ws
  word "else"
  elseBranch <- pExpr
  return $ EIf condition thenBranch elseBranch

pField = do
  expr <- pPrimary
  ws
  word "."
  field <- pIdentifierNotKeyword
  return $ EField expr field

pProj = do
  expr <- pPrimary
  ws
  word "."
  proj <- pNonEmptySpanOf isNumber
  return $ EProj expr (read proj)

pExpr :: Parser Expr
pExpr = pLet <|> pIf <|> pAbs <|> pProj <|> pField <|> pApp <|> pPrimary

pPairType = do
  word "("
  item1 <- pTypeExpr
  ws
  (TePair item1 <$> (word "," *> pTypeExpr)) <|> return item1
  -- (item1, item2) -- pair or just (expr) -- expr

pTupleType = do
  word "["
  items <- pSeparated pTypeExpr (ws <* word ",")
  ws
  word "]"
  return $ TeTuple items

pRecordType = do
  word "{"
  fields <- pSeparated pRecordFieldType (ws <* word ",")
  ws
  word "}"
  return $ TeRecord fields
  where
    pRecordFieldType = do
      var <- pIdentifierNotKeyword
      ws
      word "="
      ty <- pTypeExpr
      ws
      return (var, ty)

pAbsType = do
  fromType <- pPairType <|> pTupleType <|> pRecordType <|> pSimpleType
  ws
  word "->"
  toType <- pTypeExpr
  return $ TeAbs fromType toType

pSimpleType =
  ((\_ -> TeBool) <$> word "Bool") <|>
  ((\_ -> TeTop) <$> word "Top") <|>
  ((\_ -> TeUnit) <$> word "Unit") <|>
  ((\_ -> TeInt) <$> word "Int") <|>
  ((\_ -> TeUnknown) <$> word "?") <|>
  (word "(" *> pTypeExpr <* word ")")

pTypeExpr :: Parser TypeExpr
--pTypeExpr = pPairType <|> pTupleType <|> pRecordType <|> pAbsType <|> pSimpleType
pTypeExpr = pPairType <|> pTupleType <|> pRecordType <|> pAbsType <|> pSimpleType
-- Main

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main = undefined
