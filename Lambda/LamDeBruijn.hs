module Lambda.LamDeBruijn where

import Data.Bifunctor (second)
import Lambda.LamDef

tyMapOnVar :: (CtxId -> CtxLen -> Int -> Ty) -> Int -> Ty -> Ty
tyMapOnVar f cutoff ty =
  let walk = tyMapOnVar f
  in case ty of
    TyBoundVar id ctxLen -> f id ctxLen cutoff
    TyRec name def -> TyRec name (walk (cutoff + 1) def)
    TySome name constr repr -> TySome name (walk cutoff constr) (walk (cutoff + 1) repr)
    TyAll name constr repr -> TyAll name (walk cutoff constr) (walk (cutoff + 1) repr)

    TyFreeVar name -> TyFreeVar name
    TyUnit -> TyUnit
    TyTop -> TyTop
    TyInt -> TyInt
    TyBool -> TyBool
    TyFn from to -> TyFn (walk cutoff from) (walk cutoff to)
    TyTuple items -> TyTuple $ map (walk cutoff) items
    TyRecord fields -> TyRecord $ map (second (walk cutoff)) fields
    TyVariants variants -> TyVariants $ map (second (walk cutoff)) variants


tmMap :: (CtxId -> CtxLen -> Int -> Tm) -> (Int -> Ty -> Ty) -> Int -> Tm -> Tm
tmMap onVar onType cutoff tm =
  let walk = tmMap onVar onType
  in case tm of
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmUnit -> TmUnit
    TmInt i -> TmInt i

    TmBoundVar id ctxLen -> onVar id ctxLen cutoff

    TmIf t1 t2 t3 ->
      TmIf (walk cutoff t1) (walk cutoff t2) (walk cutoff t3)

    TmAscription t ty ->
      TmAscription (walk cutoff t) (onType cutoff ty)

    TmRecord fields ->
      TmRecord $ map (second (walk cutoff)) fields

    TmTuple items ->
      TmTuple $ map (walk cutoff) items

    TmRecordProj t l ->
      TmRecordProj (walk cutoff t) l

    TmTupleProj t i ->
      TmTupleProj (walk cutoff t) i

    TmFn x ty body ->
      TmFn x (onType cutoff ty) (walk (cutoff + 1) body)

    TmApp t1 t2 ->
      TmApp (walk cutoff t1) (walk cutoff t2)

    TmCase t branches ->
      TmCase (walk cutoff t) $
        map (
          \branch -> case branch of
            MatchTag l x body -> MatchTag l x (walk (cutoff + 1) body)
            MatchAll body -> MatchAll $ walk cutoff body
        ) branches

    TmTagged l t ty ->
      TmTagged l (walk cutoff t) (onType cutoff ty)

    TmLet x t1 t2 ->
      TmLet x (walk cutoff t1) (walk (cutoff + 1) t2)

    TmForAll name constrTy tm ->
      TmForAll name (onType cutoff constrTy) (walk (cutoff + 1) tm)

    TmConcretised tm ty ->
      TmConcretised (walk cutoff tm) (onType cutoff ty)

    TmPack ty1 tm ty2 ->
      TmPack (onType cutoff ty1) (walk cutoff tm) (onType cutoff ty2)

    TmUnpack xTy xTm tm1 tm2 ->
      TmUnpack xTy xTm (walk cutoff tm1) (walk (cutoff + 2) tm2)


tyShiftAbove d = tyMapOnVar
    (\x n cutoff -> 
      if x >= cutoff then TyBoundVar (x + d) (n + d)
      else TyBoundVar x (n + d))

tmShiftAbove d = tmMap
    (\x n cutoff -> if x >= cutoff then TmBoundVar (x + d) (n + d) else TmBoundVar x (n + d))
    (tyShiftAbove d)

tmShift d = tmShiftAbove d 0

tyShift d = tyShiftAbove d 0

bindingShift d bind =
  case bind of
    NameBind name -> NameBind name
    VarBind name ty -> VarBind name (tyShift d ty)
    TyVarBind name -> TyVarBind name
    TyAliasBind name ty -> TyAliasBind name (tyShift d ty)

-- | Substitute term `s` for the variable with index `j` in term `t`.
tmSubst j s = tmMap
    (\x n cutoff -> if x == cutoff then tmShift cutoff s else TmBoundVar x n)
    (\_ ty -> ty)
    j

-- | Substitute term `s` for index 0 in term `t` (e.g., Beta-reduction),
-- then shift everything down by 1 to eliminate the binder.
tmSubstTop s t =
  tmShift (-1) (tmSubst 0 (tmShift 1 s) t)

-- | Substitute type `tyS` for the type variable with index `j` in type `tyT`.
tySubst tyS = tyMapOnVar
    (\x n cutoff -> if x == cutoff then tyShift cutoff tyS else TyBoundVar x n)

-- | Substitute type `tyS` for type index 0 in type `tyT` (e.g., unrolling a recursive type),
-- then shift down by 1.
tySubstTop tyS tyT =
  tyShift (-1) (tySubst (tyShift 1 tyS) 0 tyT)

-- | Substitute type `tyS` for the type variable with index `j`
-- across all type annotations in term `t`.
tytmSubst tyS = tmMap
    (\x n _ -> TmBoundVar x n) -- Do not modify term variables
    (tySubst tyS)

-- | Substitute type `tyS` for type index 0 in term `t` and shift down by 1
tytmSubstTop tyS t =
  tmShift (-1) (tytmSubst (tyShift 1 tyS) 0 t)