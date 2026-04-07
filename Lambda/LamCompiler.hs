module Lambda.LamCompiler where

import Data.List (elemIndex)
import Lambda.LamDef
import Lambda.LamResults
import GHC.Exts.Heap (GenClosure(var))

type CompilerCtx = [String]

compileType :: CompilerCtx -> TypeExpr -> Compiled Ty

compileType ctx (TeVar name) =
  case elemIndex name ctx of
    Nothing -> Err $ UnboundTypeVar name
    Just index -> Ok $ TyBoundVar index (length ctx)

compileType ctx TeBool = Ok $ TyBool
compileType ctx TeInt = Ok $ TyInt
compileType ctx TeTop = Ok $ TyTop
compileType ctx TeUnit = Ok $ TyUnit

compileType ctx (TeTuple items) =
  TyTuple <$> traverse (compileType ctx) items

compileType ctx (TeRecord pairs) =
  TyRecord <$> traverse (\(name, tyExpr) -> case compileType ctx tyExpr of
    Ok ty -> Ok $ (name, ty)
    Err err -> Err err) pairs

compileType ctx (TeAbs fromTy toTy) = do
  fromTy' <- compileType ctx fromTy
  toTy' <- compileType ctx toTy
  Ok $ TyFn fromTy' toTy'

compileType ctx (TeVariants variants) =
  TyVariants <$> traverse (\(name, tyExpr) -> case compileType ctx tyExpr of
    Ok ty -> Ok $ (name, ty)
    Err err -> Err err) variants

compileType ctx (TeRec name ty) =
  TyRec name <$> (compileType (name : ctx) ty)

compileType ctx (TeAll name constrTy ty) = do
  constrTy' <- compileType ctx constrTy
  ty' <- compileType (name : ctx) ty
  Ok $ TyAll name constrTy' ty'

compileType ctx (TeSome name constrTy ty) = do
  constrTy' <- compileType ctx constrTy
  ty' <- compileType (name : ctx) ty
  Ok $ TySome name constrTy' ty'



compile :: CompilerCtx -> Expr -> Compiled Tm
compile ctx (EVar name) =
  case elemIndex name ctx of
    Nothing -> Err $ UnboundVar name
    Just index -> Ok $ TmBoundVar index (length ctx)

compile ctx (EAbs name argTy body) = do
  argTy' <- compileType ctx argTy
  body' <- compile (name : ctx) body
  Ok $ TmFn name argTy' body'

compile ctx (EApp lhs rhs) =
  TmApp <$> compile ctx lhs <*> compile ctx rhs

compile ctx (ELet name value body) = do
  val' <- compile ctx value
  body' <- compile (name : ctx) body
  Ok $ TmLet name val' body'

compile ctx (EUse absName termName expr body) = do
  e' <- compile ctx expr
  b' <- compile (termName : absName : ctx) body
  Ok $ TmUnpack absName termName e' b'

compile ctx (EIf cond thenBranch elseBranch) =
  TmIf <$> compile ctx cond <*> compile ctx thenBranch <*> compile ctx elseBranch

compile ctx (EBool "true")  = Ok TmTrue
compile ctx (EBool "false") = Ok TmFalse
compile ctx EUnit           = Ok TmUnit
compile ctx (EInt val)      = Ok $ TmInt val

compile ctx (ETuple items) =
  TmTuple <$> traverse (compile ctx) items

compile ctx (ERecord pairs) =
  TmRecord <$> traverse (\(name, expr) -> (name,) <$> compile ctx expr) pairs

compile ctx (EProj expr proj) =
  (\e -> TmTupleProj e proj) <$> compile ctx expr

compile ctx (EField expr field) =
  (\e -> TmRecordProj e field) <$> compile ctx expr

compile ctx (EAscription expr ty) =
  TmAscription <$> compile ctx expr <*> compileType ctx ty

compile ctx (ECases expr branches) = do
  e' <- compile ctx expr
  bs' <- traverse compileBranch branches
  Ok $ TmCase e' bs'
  where
    compileBranch (tag, bindingName, branchExpr) =
      MatchTag tag bindingName <$> compile (bindingName : ctx) branchExpr

compile ctx (ETagged expr tag expectedTy) =
  TmTagged tag <$> compile ctx expr <*> compileType ctx expectedTy

compile ctx (EPack ty1 expr ty2) =
  TmPack <$> compileType ctx ty1 <*> compile ctx expr <*> compileType ctx ty2

compile ctx (EForAll name constr expr) = do
  constr' <- compileType ctx constr
  body' <- compile (name : ctx) expr
  Ok $ TmForAll name constr' body'

compile ctx (EConcretised expr ty) =
  TmConcretised <$> compile ctx expr <*> compileType ctx ty

compile ctx (ESucc expr) =
  TmSucc <$> compile ctx expr

compile ctx (ETypeLet name tyExpr body) = do
  ty <- compileType ctx tyExpr
  body' <- compile (name : ctx) body
  Ok $ TmTyLet name ty body'
