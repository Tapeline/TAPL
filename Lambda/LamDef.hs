module Lambda.LamDef where

a =/= b = a /= b

type CtxLen = Int
type CtxId = Int
type VariantTag = String
type RecordTag = String
type BindingName = String

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
  | TyRec BindingName Ty
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
  | TmFn String Ty Tm
  | TmIf Tm Tm Tm
  | TmApp Tm Tm
  | TmCase Tm [CaseBranch]
  | TmTagged VariantTag Tm Ty

  | TmRecordProj Tm RecordTag
  | TmTupleProj Tm Int

  deriving (Show)


data CaseBranch
  = MatchTag VariantTag BindingName Tm
  | MatchAll Tm
  deriving (Show)


data Bind
  = NameBind BindingName
  | VarBind BindingName Ty
  | TyVarBind BindingName
  | TyAliasBind BindingName Ty
  deriving (Show)


type Ctx = [Bind]


addBind :: Ctx -> Bind -> Ctx
addBind ctx bind = bind : ctx


getBind :: Ctx -> Int -> Maybe Bind
getBind [] index = Nothing
getBind (x : _) 0 = Just x
getBind (_ : rest) index = getBind rest (index - 1)


indexToName :: Ctx -> Int -> String
indexToName ctx id =
  case getBind ctx id of
    Just (NameBind name) -> name
    Just (VarBind name _) -> name
    Just (TyVarBind name) -> name
    Nothing -> error "broken context: couldn't find name for var@" ++ show id


data TypeError
  = NameNotFound Ctx CtxId
  | AliasNotFound Ctx CtxId
  | NotATypeAlias String
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
  | NotImplemented
