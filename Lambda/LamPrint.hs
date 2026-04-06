module Lambda.LamPrint where

import Lambda.LamDef
import Data.List (intercalate)
import GHC.Base (bindIO)

isNameBound :: Ctx -> String -> Bool
isNameBound [] name = False
isNameBound (x:xs) name =
  anyMatched || isNameBound xs name
  where
    anyMatched = case x of
      NameBind n -> name == n
      VarBind n _ -> name == n
      TyVarBind n -> name == n
      TyAliasBind n _ -> name == n

pickFreshName ctx name =
  if isNameBound ctx name then pickFreshName ctx (name ++ "'")
  else ((NameBind name) : ctx, name)

printTy :: Ctx -> Ty -> String
printTy ctx ty = case ty of
  TyFreeVar name -> name
  TyBoundVar id _ -> indexToName ctx id
  TyUnit -> "Unit"
  TyTop -> "Top"
  TyInt -> "Int"
  TyBool -> "Bool"

  TyFn ty1 ty2 ->
    "(" ++ walk ty1 ++ " -> " ++ walk ty2 ++ ")"

  TyTuple tys ->
    "[" ++ intercalate ", " (map walk tys) ++ "]"

  TyRecord fields ->
    "{" ++ intercalate ", " (map (\(l, t) -> l ++ ": " ++ walk t) fields) ++ "}"

  TyVariants variants ->
    "<" ++ intercalate ", " (map (\(l, t) -> l ++ ": " ++ walk t) variants) ++ ">"

  TyRec name ty ->
    let (ctx', name') = pickFreshName ctx name in
    "μ " ++ name ++ ": " ++ printTy ctx' ty

  where walk = printTy ctx

printTm :: Ctx -> Tm -> String
printTm ctx tm = case tm of
  TmTrue -> "True"
  TmFalse -> "False"
  TmUnit -> "Unit"
  TmInt i -> show i
  TmBoundVar id _ -> indexToName ctx id

  TmIf t1 t2 t3 ->
    "if " ++ walk t1 ++
    " then " ++ walk t2 ++
    " else " ++ walk t3

  TmAscription t ty ->
    "(" ++ walk t ++ " as " ++ printTy ctx ty ++ ")"

  TmRecord fields ->
    "{" ++ intercalate ", " (map (\(l, t) -> l ++ "=" ++ walk t) fields) ++ "}"

  TmTuple tms ->
    "{" ++ intercalate ", " (map (walk) tms) ++ "}"

  TmRecordProj t label ->
    walk t ++ "." ++ label

  TmTupleProj t i ->
    walk t ++ "." ++ show i

  TmFn x ty body ->
    "(λ" ++ x ++ ":" ++ printTy ctx ty ++ ". " ++ printTm (addBind ctx (VarBind x ty)) body ++ ")"

  TmApp t1 t2 ->
    "(" ++ walk t1 ++ " " ++ walk t2 ++ ")"

  TmLet x t1 t2 ->
    "let " ++ x ++ " = " ++ walk t1 ++ " in " ++ printTm (addBind ctx (NameBind x)) t2

  TmTagged label t ty ->
    "<" ++ label ++ "=" ++ walk t ++ "> as " ++ printTy ctx ty

  TmCase t branches ->
    "case " ++ walk t ++ " of " ++ intercalate " | " (map printBranch branches)
    where
      printBranch (MatchTag label x body) =
        "<" ++ label ++ "=" ++ x ++ "> => " ++ printTm (addBind ctx (NameBind x)) body
      printBranch (MatchAll body) =
        "_ => " ++ walk body

  where walk = printTm ctx



instance Show TypeError where
  show (NameNotFound ctx id) = "name not found by index " ++ show id ++ " in " ++ show ctx
  show (UntypeableBind name) = "untypeable bind " ++ name
  show (AliasNotFound ctx id) = "alias not found by index " ++ show id ++ " in " ++ show ctx
  show (NotATypeAlias name) = name ++ " is not a type alias"
  show (IncompatibleAscription ctx actual attempted) =
    "cannot ascribe " ++ printTy ctx actual ++ " with " ++ printTy ctx attempted
  show (BranchesTypesDiffer ctx tys) = "branches types differ: " ++ intercalate ", " (map (printTy ctx) tys)
  show (ConditionNotBool ctx ty) = "condition " ++ printTy ctx ty ++ " is not a bool"
  show (NotCallable ctx ty) = printTy ctx ty ++ " is not callable"
  show (NotApplicable ctx tyA tyB) = printTy ctx tyA ++ " is not applicable to " ++ printTy ctx tyB
  show (NotAVariantType ctx ty) = printTy ctx ty ++ " is not variant type"
  show (VariantNotMatched ctx tag ty expectedTy) =
    "on tag " ++ tag ++ " expected " ++ printTy ctx expectedTy ++ ", but got " ++ printTy ctx ty
  show (VariantNotFound tag) = "variant " ++ tag ++ " not found"
  show (VariantsNotExhausted tags) =
    "variants " ++ intercalate ", " (map (\tag -> "\""++tag++"\"") tags) ++ " are not exhausted"
  show (UnknownRecordProj ctx tag ty) =
    "projection " ++ tag ++ " is non-existent in " ++ printTy ctx ty
  show (UnknownTupleProj ctx index ty) =
    "projection " ++ show index ++ " is non-existent in " ++ printTy ctx ty
  show (UnknownCaseTag tag) = "variant " ++ tag ++ " not found"
  show NotImplemented = "this feature is not implemented"
