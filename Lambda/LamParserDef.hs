module Lambda.LamParserDef where

import Control.Applicative
import Data.Char
import Data.Set (Set, fromList, member)
import qualified Data.Text as T
import Distribution.Compat.Prelude (readMaybe)
import Lambda.LamDef
import Lambda.LamResults
import System.FilePath (isValid)

-- Parser def

-- Parser of a is a thing that can read a source string,
-- then return Nothing or rest source and a parsed thing.
newtype Parser a = Parser
  { runParser :: ParserState -> Parsed a
  }

raise :: ParserState -> ParserErrorType -> Parsed a
raise state err = Err $ ParserError (currentLine state) (currentCol state) err


-- For each Parser<a> we can inject a function a->b and get Parser<b>
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', a) <- p input
      Ok (input', f a)

-- For each two parsers (a and b), we can apply them sequentially and
-- give back result of (a b) application.
-- For each single parser, we can just apply it to code.
instance Applicative Parser where
  pure a =
    Parser $ \input ->
      Ok (input, a)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', a) <- p1 input
      (input'', b) <- p2 input'
      Ok (input'', a b)

-- For each two parsers we can apply first one and then second,
-- if first has failed to parse.
instance Alternative Parser where
  empty = Parser $ \input -> raise input EmptyInput
  (Parser p1) <|> (Parser p2) =
    Parser $ \state ->
      case p1 state of
        Ok result -> Ok result
        Err _ -> p2 state

instance Monad Parser where
  return = pure
  (Parser p) >>= somethingAfter = Parser $ \input -> do
    (input', a) <- p input
    let nextParser = somethingAfter a
    runParser nextParser input'

-- Parser impl

incStateByChar :: ParserState -> Char -> ParserState
incStateByChar state char =
  if char `elem` "\n\r"
    then ParserState newText (currentLine state + 1) 1
    else ParserState newText (currentLine state) (currentCol state + 1)
  where
    newText = case T.uncons (input state) of
      Just (_, t) -> t
      _ -> T.empty

incStateByText :: ParserState -> T.Text -> ParserState
incStateByText state text = T.foldl' incStateByChar state text

pChar :: Char -> Parser Char
pChar char = Parser $ \state ->
  case T.uncons $ input state of
    Just (headChar, tailText) | headChar == char ->
      Ok $ (incStateByChar state headChar, headChar)
    Just (headChar, _) | headChar =/= char ->
      raise state (ExpectedButGot [char] [headChar])
    Nothing ->
      raise state (ExpectedButGot [char] "NOTHING")

-- here I surrendered:

pString :: String -> Parser String
pString = traverse pChar

-- | Eat all heading characters applicable to predicate
pSpanOf :: (Char -> Bool) -> Parser T.Text
pSpanOf predicate = Parser $ \state ->
    let (consumed, rest) = T.span predicate (input state)
        newState = incStateByText state consumed
        finalState = newState { input = rest }
    in Ok (finalState, consumed)

pNonEmptySpanOf :: (Char -> Bool) -> Parser T.Text
pNonEmptySpanOf predicate = Parser $ \state ->
    let (consumed, rest) = T.span predicate (input state)
    in if T.null consumed
       then raise state (ExpectedButGot "something" "nothing")
       else let newState = incStateByText state consumed
                finalState = newState { input = rest }
            in Ok (finalState, consumed)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \state -> do
    (state', something) <- p state
    if null something
    then raise state (ExpectedButGot "non-empty list" "empty list")
    else Ok (state', something)

-- | Eat all whitespaces
ws = pSpanOf isSpace

-- | Eat trailing whitespace
token :: Parser a -> Parser a
token p = p <* ws

word :: String -> Parser String
word s = token (pString s)

-- | Parses one or more occurrences of p, separated by sep
pSeparated1 :: Parser a -> Parser s -> Parser [a]
pSeparated1 p sep = (:) <$> p <*> many (sep *> p)

-- | Parses zero or more occurrences of p, separated by sep
pSeparated :: Parser a -> Parser s -> Parser [a]
pSeparated p sep = pSeparated1 p sep <|> pure []


(<?>) :: Parser a -> ParserErrorType -> Parser a
(Parser p) <?> err = Parser $ \state ->
  case p state of
    Err e -> Err (e { Lambda.LamDef.error = err })
    Ok res -> Ok res

parseFile :: FilePath -> Parser a -> IO (Result ParserError a)
parseFile fileName parser = do
  inputStr <- readFile fileName
  return $ snd <$> runParser parser (ParserState (T.pack inputStr) 1 1)
