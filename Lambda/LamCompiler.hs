module Lambda.LamCompiler where

import Data.List
import Lambda.LamParser
import Lambda.LamDef

type CompilerCtx = [String]

compile :: CompilerCtx -> Expr -> Tm
compile ctx (EVar name) =
  case foundName of
    Nothing -> error $ "unbound var " ++ name
    Just index -> TmBoundVar index (length ctx)
  where
    foundName = elemIndex name ctx
compile ctx (EAbs name argTy body) =
  TmFn name (compileType argTy) (compile (name : ctx) body)
compile ctx (ELet name value body) =
  TmLet name (compile ctx value) (compile (name : ctx) body)
compile ctx (EApp lhs rhs) =
  TmApp (compile ctx lhs) (compile ctx rhs)
compile ctx (EBool "True") = TmTrue
compile ctx (EBool "False") = TmFalse
compile ctx EUnit = TmUnit
compile ctx (EField expr field) =
  TmRecordProj (compile ctx expr) field
compile ctx (EProj expr proj) =
  TmTupleProj (compile ctx expr) proj
compile ctx (EIf cond thenBranch elseBranch) =
  TmIf (compile ctx cond) (compile ctx thenBranch) (compile ctx elseBranch)
compile ctx (ERecord pairs) =
  TmRecord $ map (\(name, expr) -> (name, compile ctx expr)) pairs
compile ctx (ETuple items) =
  TmTuple $ map (compile ctx) items
compile ctx (EInt val) = TmInt val

compileType TeBool = TyBool
compileType TeTop = TyTop
compileType TeUnit = TyUnit
compileType TeInt = TyInt
compileType (TeAbs fromTy toTy) =
  TyFn (compileType fromTy) (compileType toTy)
compileType (TeTuple items) =
  TyTuple $ map compileType items
compileType (TeRecord pairs) =
  TyRecord $ map (\(name, tyExpr) -> (name, compileType tyExpr)) pairs
