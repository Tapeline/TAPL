module Lambda.LamDef where

import Lambda.LamResults

import Data.Text (Text)


a =/= b = a /= b

type PosInfo = (Int, Int, Int) -- line, char, length

type CtxLen = Int

type CtxId = Int

type VariantTag = String

type RecordTag = String

type BindingName = String
type TyBindingName = String

data Ty
  = TyFreeVar String
  | TyBoundVar CtxId CtxLen
  | TyUnit
  | TyTop
  | TyInt
  | TyBool
  | TyFn Ty Ty
  | TyTuple [Ty]
  | TyRecord [(RecordTag, Ty)]
  | TyVariants [(VariantTag, Ty)]
  | TyRec TyBindingName Ty
  | TyAll TyBindingName Ty Ty -- A constraint : type
  | TySome TyBindingName Ty Ty -- E constraint : type
  deriving (Show, Eq)

data Tm
  = TmTrue
  | TmFalse
  | TmUnit
  | TmInt Int
  | TmBoundVar CtxId CtxLen
  | TmAscription Tm Ty
  | TmTuple [Tm]
  | TmRecord [(RecordTag, Tm)]
  | TmLet BindingName Tm Tm
  | TmTyLet BindingName Ty Tm
  | TmFn String Ty Tm
  | TmIf Tm Tm Tm
  | TmApp Tm Tm
  | TmCase Tm [CaseBranch]
  | TmTagged VariantTag Tm Ty
  | TmRecordProj Tm RecordTag
  | TmTupleProj Tm Int
  | TmSucc Tm

  -- | Create an existential type construction:
  -- 1. Ty -- actual (representation) type of an ADT
  -- 2. Tm -- implementation term of an ADT
  -- 3. TySome -- ADT's "disguise" type
  | TmPack Ty Tm Ty

  -- | Unpacking of an existential type:
  -- 1. String -- local name of abstract type
  -- 2. String -- local name of abstracted value,
  -- 3. Tm -- unpacking target
  -- 4. Tm -- term that uses bound names
  | TmUnpack TyBindingName BindingName Tm Tm

  -- | Introduce universal (forall) type: `∀ X <: T. t`
  -- 1. String X -- name
  -- 2. Ty T -- type bound (hereinafter -- constraint)
  -- 3. Tm t -- quantified term
  | TmForAll TyBindingName Ty Tm --

  -- | Concretise universal type
  -- 1. Tm -- quantified term
  -- 2. Ty -- concrete type to be substituted
  | TmConcretised Tm Ty
  deriving (Show)

data CaseBranch
  = MatchTag VariantTag BindingName Tm
  | MatchAll Tm
  deriving (Show)

data Bind
  -- | A free variable
  = NameBind BindingName
  -- | A bound variable
  | VarBind BindingName Ty
  -- | A free type variable
  | TyVarBind TyBindingName
  -- | Alias a type
  | TyAliasBind TyBindingName Ty
  -- | Some type that is <: Ty
  | TyConstrainedBind TyBindingName Ty
  deriving (Show)

type Ctx = [Bind]

data TypeError
  = NameNotFound Ctx CtxId
  | AliasNotFound Ctx CtxId
  | ConstrainedTypeNotFound Ctx CtxId
  | NotATypeAlias String
  | NotAConstrainedType String
  | UntypeableBind BindingName
  | IncompatibleAscription Ctx Ty Ty
  | ConditionNotBool Ctx Ty
  | BranchesTypesDiffer Ctx [Ty]
  | NotCallable Ctx Ty
  | NotApplicable Ctx Ty Ty
  | NotAVariantType Ctx Ty
  | VariantNotMatched Ctx VariantTag Ty Ty
  | VariantNotFound VariantTag
  | UnknownRecordProj Ctx RecordTag Ty
  | UnknownTupleProj Ctx Int Ty
  | UnknownCaseTag VariantTag
  | VariantsNotExhausted [VariantTag]
  | ExpectedExistential Ctx Ty
  | ExpectedUniveral Ctx Ty
  | NotASubtype Ctx Ty Ty
  | EscapedVar Ctx Ty
  | NotImplemented

data ParserErrorType
  = ExpectedButGot String String
  | ParsingFailed
  | EmptyInput
  deriving (Show)

data ParserState = ParserState
  { input :: Text
  , currentLine :: Int
  , currentCol :: Int
  }

data ParserError = ParserError
  { line :: Int
  , col :: Int
  , error :: ParserErrorType
  }

data Expr
  = EVar String
  | EAbs String TypeExpr Expr
  | EApp Expr Expr
  | ELet String Expr Expr
  | EUse String String Expr Expr
  | EIf Expr Expr Expr
  | EBool String
  | EUnit
  | ETuple [Expr]
  | ERecord [(String, Expr)]
  | EProj Expr Int
  | EField Expr String
  | EInt Int
  | EAscription Expr TypeExpr
  | ECases Expr [(String, String, Expr)]
  | ETagged Expr String TypeExpr
  | EPack TypeExpr Expr TypeExpr
  | EForAll String TypeExpr Expr
  | EConcretised Expr TypeExpr
  | ESucc Expr
  | ETypeLet String TypeExpr Expr
  deriving (Show)

data TypeExpr
  = TeBool
  | TeInt
  | TeTop
  | TeUnit
  | TeTuple [TypeExpr]
  | TeRecord [(String, TypeExpr)]
  | TeAbs TypeExpr TypeExpr
  | TeVariants [(String, TypeExpr)]
  | TeRec String TypeExpr
  | TeAll String TypeExpr TypeExpr
  | TeSome String TypeExpr TypeExpr
  | TeVar String
  deriving (Show)

type Parsed a = Result ParserError (ParserState, a)

data CompilerError
  = UnboundVar String
  | UnboundTypeVar String

type Compiled a = Result CompilerError a

data RuntimeError
  = NoRuleApplies
  deriving (Show, Eq)

type Evaluated a = Result RuntimeError a
