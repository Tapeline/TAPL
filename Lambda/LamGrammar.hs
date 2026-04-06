module Lambda.LamGrammar where
import Control.Applicative
import Data.Char
import Data.Set (Set, member, fromList)
import System.FilePath (isValid)
import Distribution.Compat.Prelude (readMaybe)
import Lambda.LamDef
import Lambda.LamResults
import Lambda.LamParserDef
import qualified Data.Text as T

-- Grammar impl

isReservedKeyword kw = member kw $ fromList ["if", "let", "then", "else", "True", "Unit", "False"]

isValidIdChar '\'' = True
isValidIdChar '_' = True
isValidIdChar someChar = isAlphaNum someChar

pIdentifierNotKeyword = do
    identText <- pSpanOf isValidIdChar
    let ident = T.unpack identText
    if null ident
      then Parser $ \state -> raise state (ExpectedButGot "identifier" "nothing")
      else if isReservedKeyword ident
        then Parser $ \state -> raise state (ExpectedButGot "identifier" ("reserved keyword: " ++ ident))
        else return ident
