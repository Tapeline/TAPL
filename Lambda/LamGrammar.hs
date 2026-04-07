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

isReservedKeyword kw = member kw $ fromList [
  "if", "let", "then", "else", "true", "unit", "false",
  "as", "case", "of", "use", "forall", "constrained",
  "thereis", "rec"]

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

pPrimary :: Parser Expr
pPrimary =
  EInt . read . T.unpack <$> pNonEmptySpanOf isNumber <|>
  EVar <$> pIdentifierNotKeyword <|>
  EBool <$> word "true" <|>
  EBool <$> word "false" <|>
  EUnit <$ word "unit" <|>
  pPacked <|>
  pTagged <|>
  pForAll <|>
  pTuple <|>
  pRecord <|>
  word "(" *> pExpr <* word ")"

pForAll = do
  word "forall"
  tyName <- pIdentifierNotKeyword
  ws
  word "<:"
  constraintTy <- pTypeExpr
  ws
  word "."
  EForAll tyName constraintTy <$> pExpr

pPacked = do
  word "{*"
  actualTy <- pTypeExpr
  ws
  word ","
  implExpr <- pExpr
  ws
  word "}"
  word "as"
  EPack actualTy implExpr <$> pTypeExpr

pTagged = do
  word "<"
  tag <- pIdentifierNotKeyword
  ws
  word "="
  expr <- pExpr
  word ">"
  word ":"
  ty <- pTypeExpr
  return $ ETagged expr tag ty

pTuple = do
  word "["
  items <- pSeparated pExpr (ws <* word ",")
  ws
  word "," <|> pure ""
  word "]"
  return $ ETuple items

pRecord = do
  word "{"
  fields <- pSeparated pRecordField (ws <* word ",")
  ws
  word "," <|> pure ""
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
    EAbs boundVar ty <$> pExpr

pAppExpr = do
    function <- pPostfixExpr
    ws -- Parse all consequent terms
    arguments <- many (ws *> pPostfixExpr <* ws)
    return $ foldl EApp function arguments -- Fold 'em right-associatively

pTypeLet = do
  word "let"
  word "type"
  boundVar <- pIdentifierNotKeyword
  ws
  word "="
  ty <- pTypeExpr
  ws
  word "," <|> word "in"
  ETypeLet boundVar ty <$> pExpr


pLet = do
  word "let"
  boundVar <- pIdentifierNotKeyword
  ws
  word "="
  boundExpr <- pExpr
  ws
  word "," <|> word "in"
  ELet boundVar boundExpr <$> pExpr

pUse = do
  word "use"
  boundVar <- pIdentifierNotKeyword
  ws
  word ":"
  typeBoundVar <- pIdentifierNotKeyword
  ws
  word "="
  boundExpr <- pExpr
  ws
  word "," <|> word "in"
  EUse typeBoundVar boundVar boundExpr <$> pExpr

pIf = do
  word "if"
  condition <- pExpr
  ws
  word "then"
  thenBranch <- pExpr
  ws
  word "else"
  EIf condition thenBranch <$> pExpr

pCase = do
  word "case"
  expr <- pExpr
  ws
  word "of"
  word "|"
  cases <- pSeparated pCaseBranch (ws <* word "|")
  return $ ECases expr cases
  where
    pCaseBranch = do
      tag <- pIdentifierNotKeyword
      ws
      binding <- pIdentifierNotKeyword
      ws
      word "->"
      expr <- pExpr
      return (tag, binding, expr)

pPostfixExpr = do
  expr <- pPrimary
  ops <- many (ws *> pPostfix <* ws)
  return $ foldl (\e op -> op e) expr ops

pPostfix = pFinishProj <|> pFinishField <|> pFinishAscription <|> pFinishConcresitation

pFinishProj = do
  word "."
  flip EProj . read . T.unpack <$> pNonEmptySpanOf isNumber

pFinishField = do
  word "."
  flip EField <$> pIdentifierNotKeyword

pFinishAscription = do
  word "as"
  ws
  flip EAscription <$> pTypeExpr

pFinishConcresitation = do
  word "concretised for"
  ws
  flip EConcretised <$> pTypeExpr

pBuiltinCallables =
  ESucc <$> (word "succ" *> pExpr)

pExpr =
  pTypeLet <|>
  pLet <|>
  pUse <|>
  pIf <|>
  pCase <|>
  pAbs <|>
  pBuiltinCallables <|>
  pAppExpr


pTupleType = do
  word "["
  items <- pSeparated pTypeExpr (ws <* word ",")
  ws
  word "," <|> pure ""
  word "]"
  return $ TeTuple items

pRecordType = do
  word "{"
  fields <- pSeparated pRecordFieldType (ws <* word ",")
  ws
  word "," <|> pure ""
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

pVariantType = do
  word "<"
  fields <- pSeparated pVariant (ws <* word ",")
  ws
  word "," <|> pure ""
  word ">"
  return $ TeVariants fields
  where
    pVariant = do
      var <- pIdentifierNotKeyword
      ws
      word "="
      ty <- pTypeExpr
      ws
      return (var, ty)

pAbsType = do
  fromType <- pTypeExpr'NotAbs
  ws
  word "->"
  TeAbs fromType <$> pTypeExpr

pSimpleType =
  const TeBool <$> word "Bool" <|>
  const TeTop <$> word "Top" <|>
  const TeUnit <$> word "Unit" <|>
  const TeInt <$> word "Int" <|>
  TeVar <$> pIdentifierNotKeyword <|>
  word "(" *> pTypeExpr <* word ")"

pRecType = do
  word "rec"
  tyName <- pIdentifierNotKeyword
  ws
  word "."
  tyExpr <- pTypeExpr
  return $ TeRec tyName tyExpr

pForAllType = do
  word "forall"
  tyName <- pIdentifierNotKeyword
  ws
  word "<:"
  constraintTy <- pTypeExpr
  ws
  word "."
  tyExpr <- pTypeExpr
  return $ TeAll tyName constraintTy tyExpr

pThereIsSomeType = do
  word "thereis"
  tyName <- pIdentifierNotKeyword
  ws
  word "<:"
  constraintTy <- pTypeExpr
  ws
  word "."
  tyExpr <- pTypeExpr
  return $ TeSome tyName constraintTy tyExpr

pTypeExpr'NotAbs =
  pVariantType <|> pTupleType <|> pRecordType <|>
  pRecType <|>
  pForAllType <|> pThereIsSomeType <|>
  pSimpleType

pTypeExpr = pAbsType <|> pTypeExpr'NotAbs
