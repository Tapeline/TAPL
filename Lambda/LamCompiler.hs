module Lambda.LamCompiler where

import Data.List
import Lambda.LamParser
import Lambda.LamRuntime

type CompilerCtx = [String]

compile :: CompilerCtx -> Expr -> Tm
compile ctx (EVar name) =
  case foundName of
    Nothing -> error $ "unbound var " ++ name
    Just index -> TmBoundVar index (length ctx)
  where
    foundName = elemIndex name ctx
compile ctx (EAbs name argTy body) =
  TmAbs name (compileType argTy) (compile (name : ctx) body)
compile ctx (ELet name value body) =
  TmLet name (compile ctx value) (compile (name : ctx) body)
compile ctx (EApp lhs rhs) =
  TmApp (compile ctx lhs) (compile ctx rhs)
compile ctx (EBool "True") = TmTrue
compile ctx (EBool "False") = TmFalse
compile ctx EUnit = TmUnit
compile ctx (EField expr field) =
  TmField field (compile ctx expr)
compile ctx (EProj expr proj) =
  TmProj proj (compile ctx expr)
compile ctx (EIf cond thenBranch elseBranch) =
  TmIf (compile ctx cond) (compile ctx thenBranch) (compile ctx elseBranch)
compile ctx (EPair a b) =
  TmPair (compile ctx a) (compile ctx b)
compile ctx (ERecord pairs) =
  TmRecord $ map (\(name, expr) -> (name, compile ctx expr)) pairs
compile ctx (ETuple items) =
  TmTuple $ map (compile ctx) items
compile ctx (EInt val) = TmInt val

compileType TeBool = TyBool
compileType TeTop = TyTop
compileType TeUnit = TyUnit
compileType TeUnknown = TyUnknown
compileType TeInt = TyInt
compileType (TeAbs fromTy toTy) =
  TyFn (compileType fromTy) (compileType toTy)
compileType (TePair a b) =
  TyPair (compileType a) (compileType b)
compileType (TeTuple items) =
  TyTuple $ map compileType items
compileType (TeRecord pairs) =
  TyRecord $ map (\(name, tyExpr) -> (name, compileType tyExpr)) pairs
