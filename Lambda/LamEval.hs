module Lambda.LamEval where

import Lambda.LamDef
import Lambda.LamDeBruijn
import Lambda.LamResults


isFinal :: Tm -> Bool
isFinal tm = case tm of
  TmTrue -> True
  TmFalse -> True
  TmInt _ -> True
  TmUnit -> True
  TmFn {} -> True
  TmForAll {} -> True
  TmTuple items -> all isFinal items
  TmRecord fields -> all (isFinal . snd) fields
  TmTagged _ t _ -> isFinal t
  TmPack _ t _ -> isFinal t
  _ -> False


evalOnce :: Ctx -> Tm -> Evaluated Tm
evalOnce ctx tm = case tm of
  -- 1. Standard Reductions (Let, If, Succ)
  TmLet name def body | isFinal def -> Ok $ tmSubstTop def body
  TmLet name def body -> do
    def' <- evalOnce ctx def
    Ok $ TmLet name def' body

  TmIf TmTrue thenTm _ -> Ok thenTm
  TmIf TmFalse _ elseTm -> Ok elseTm
  TmIf cond t e -> TmIf <$> evalOnce ctx cond <*> Ok t <*> Ok e

  TmSucc (TmInt x) -> Ok $ TmInt (x + 1)
  TmSucc t -> TmSucc <$> evalOnce ctx t

  TmAscription t _ | isFinal t -> Ok t
  TmAscription t ty -> (`TmAscription` ty) <$> evalOnce ctx t

  TmApp (TmFn _ _ body) arg | isFinal arg -> Ok $ tmSubstTop arg body
  TmApp fn arg | isFinal fn -> TmApp fn <$> evalOnce ctx arg
  TmApp fn arg -> TmApp <$> evalOnce ctx fn <*> Ok arg

  TmTuple items -> TmTuple <$> evalList items
  TmTupleProj (TmTuple items) i | isFinal (TmTuple items) ->
    if i < length items then Ok (items !! i) else Err NoRuleApplies
  TmTupleProj t i -> (`TmTupleProj` i) <$> evalOnce ctx t

  TmRecord fields -> TmRecord <$> evalFields fields
  TmRecordProj (TmRecord fields) l | isFinal (TmRecord fields) ->
    case lookup l fields of
      Just v -> Ok v
      Nothing -> Err NoRuleApplies
  TmRecordProj t l -> (`TmRecordProj` l) <$> evalOnce ctx t

  TmTagged l t ty -> (\t' -> TmTagged l t' ty) <$> evalOnce ctx t
  TmCase (TmTagged l v _) branches | isFinal v -> evalCase l v branches
  TmCase t branches -> (`TmCase` branches) <$> evalOnce ctx t

  TmConcretised (TmForAll _ _ body) ty -> Ok $ tytmSubstTop ty body
  TmConcretised t ty -> (`TmConcretised` ty) <$> evalOnce ctx t

  TmUnpack _ _ (TmPack tyV v _) body | isFinal v ->
    Ok $ tytmSubstTop tyV (tmSubstTop v body)
  TmPack ty1 t ty2 -> (\t' -> TmPack ty1 t' ty2) <$> evalOnce ctx t
  TmUnpack xTy xTm t body -> (\t' -> TmUnpack xTy xTm t' body) <$> evalOnce ctx t

  TmTyLet _ ty body -> Ok $ tytmSubstTop ty body

  _ -> Err NoRuleApplies


-- | Evaluate the first non-final element in a list
evalList :: [Tm] -> Evaluated [Tm]
evalList [] = Err NoRuleApplies
evalList (t:ts)
  | isFinal t = (t:) <$> evalList ts
  | otherwise = (:ts) <$> evalOnce [] t

-- | Evaluate the first non-final field in a record
evalFields :: [(RecordTag, Tm)] -> Evaluated [(RecordTag, Tm)]
evalFields [] = Err NoRuleApplies
evalFields ((l, t):fs)
  | isFinal t = ((l, t):) <$> evalFields fs
  | otherwise = (\t' -> (l, t'):fs) <$> evalOnce [] t

-- | Handle the branch logic for TmCase
evalCase :: VariantTag -> Tm -> [CaseBranch] -> Evaluated Tm
evalCase l v [] = Err NoRuleApplies
evalCase l v (MatchTag tag name body : rest)
  | l == tag = Ok $ tmSubstTop v body
  | otherwise = evalCase l v rest
evalCase l v (MatchAll body : _) = Ok body

eval :: Ctx -> Tm -> Evaluated Tm
eval ctx tm =
  case evalOnce ctx tm of
    Ok tm' -> eval ctx tm'
    Err NoRuleApplies -> Ok tm
