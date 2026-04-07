{- HLINT ignore "Redundant if" -}
module Lambda.LamTypecheck where
import Lambda.LamDef
import Lambda.LamResults
import Lambda.LamDeBruijn
import Data.Bifunctor (second)
import Distribution.Compat.Lens (_1)
import GHC.Exts.Heap (GenClosure(var))
import Control.Monad (when)

type Typechecked = Result TypeError Ty


isAnAlias ctx id = case getBind ctx id of
  Just (TyAliasBind _ _) -> True
  _ -> False


resolveAlias ctx id = case getBind ctx id of
  Just (TyAliasBind _ ty) -> Ok ty
  Just _ -> Err $ NotATypeAlias $ indexToName ctx id
  _ -> Err $ AliasNotFound ctx id


isConstrained ctx id = case getBind ctx id of
  Just (TyConstrainedBind _ _) -> True
  _ -> False


resolveConstrained ctx id = case getBind ctx id of
  Just (TyConstrainedBind _ ty) -> Ok ty
  Just _ -> Err $ NotAConstrainedType $ indexToName ctx id
  _ -> Err $ ConstrainedTypeNotFound ctx id


simplifyTy ctx ty =
  case computeTy ctx ty of
    Err _ -> ty
    Ok ty' -> simplifyTy ctx ty'
  where
    computeTy ctx recType@(TyRec name def) = Ok $ tySubstTop recType def
    computeTy ctx (TyBoundVar id _) = resolveAlias ctx id
    computeTy ctx _ = Err NotImplemented


areEquivalent ctx tyS tyT = eqv [] ctx tyS tyT where
  eqv seen ctx tyS tyT =
    elem (tyS, tyT) seen ||  -- if pair was seen, it's equivalent
    case (tyS, tyT) of       -- or else apply equivalence rules
      (TyUnit, TyUnit) -> True
      (TyTop, TyTop) -> True
      (TyInt, TyInt) -> True
      (TyBool, TyBool) -> True
      (TyRec name tyS', _) ->
        eqv markSeen ctx (tySubstTop tyS tyS') tyT
      (_, TyRec name tyT') ->
        eqv markSeen ctx tyS (tySubstTop tyT tyT')
      (TyFreeVar nameS, TyFreeVar nameT) ->
        nameS == nameT
      (TyBoundVar id _, _) | isAnAlias ctx id ->
        case resolveAlias ctx id of
          Ok resolved -> eqv seen ctx resolved tyT
          Err _ -> False
      (_, TyBoundVar id _) | isAnAlias ctx id ->
        case resolveAlias ctx id of
          Ok resolved -> eqv seen ctx tyS resolved
          Err _ -> False
      (TyBoundVar idS _, TyBoundVar idT _) ->
        idS == idT
      (TyFn tySFrom tySTo, TyFn tyTFrom tyTTo) ->
        eqv seen ctx tySFrom tyTFrom && eqv seen ctx tySTo tyTTo
      (TyTuple itemsS, TyTuple itemsT) ->
        not (length itemsS =/= length itemsT) && and (zipWith (eqv seen ctx) itemsS itemsT)
      (TyRecord fieldsS, TyRecord fieldsT) ->
        not (length fieldsS =/= length fieldsT) && all (
          \(nameS, tyS) -> case lookup nameS fieldsT of
            Nothing -> False
            Just tyT -> eqv seen ctx tyS tyT
        ) fieldsS
      (TyVariants variantsS, TyVariants variantsT) ->
        not (length variantsS =/= length variantsT) && all (
          \(nameS, tyS) -> case lookup nameS variantsT of
            Nothing -> False
            Just tyT -> eqv seen ctx tyS tyT
        ) variantsS
      (TySome name constrS tyS, TySome _ constrT tyT) ->
        eqv seen ctx constrS constrT && eqv seen (addBind ctx (NameBind name)) tyS tyT
      (TyAll name constrS tyS, TyAll _ constrT tyT) ->
        eqv seen ctx constrS constrT && eqv seen (addBind ctx (NameBind name)) tyS tyT
      _ -> False
    where
      markSeen = (tyS, tyT) : seen


-- | Is S <: T?
-- isSubtype ctx S T
isSubtype ctx tyS tyT = subtype [] ctx tyS tyT where
  subtype seen ctx tyS tyT =
    elem (tyS, tyT) seen ||
    case (tyS, tyT) of
      (_, TyTop) -> True
      (TyUnit, TyUnit) -> True
      (TyInt, TyInt) -> True
      (TyBool, TyBool) -> True
      (TyFreeVar nameS, TyFreeVar nameT) -> nameS == nameT
      (TyBoundVar idS _, TyBoundVar idT _) | idS == idT -> True
      (TyBoundVar id _, _) | isAnAlias ctx id ->
        case resolveAlias ctx id of
          Ok resolved -> subtype seen ctx resolved tyT
          Err _ -> False
      (_, TyBoundVar id _) | isAnAlias ctx id ->
        case resolveAlias ctx id of
          Ok resolved -> subtype seen ctx tyS resolved
          Err _ -> False
      (TyBoundVar id _, _) | isConstrained ctx id ->
        case resolveConstrained ctx id of
          Ok boundTy -> subtype seen ctx boundTy tyT
          Err _ -> False
      (TyRec name tyS', _) ->
        subtype markSeen ctx (tySubstTop tyS tyS') tyT
      (_, TyRec name tyT') ->
        subtype markSeen ctx tyS (tySubstTop tyT tyT')
      (TyFn tySFrom tySTo, TyFn tyTFrom tyTTo) ->
        subtype seen ctx tyTFrom tySFrom && subtype seen ctx tySTo tyTTo
      (TyRecord fieldsS, TyRecord fieldsT) ->
        all (\(nameT, tyT) ->
          case lookup nameT fieldsS of
            Nothing -> False
            Just tyS -> subtype seen ctx tyS tyT
        ) fieldsT
      (TyTuple itemsS, TyTuple itemsT) ->
        length itemsS >= length itemsT &&
        and (zipWith (subtype seen ctx) itemsS itemsT)
      (TyVariants variantsS, TyVariants variantsT) ->
        all (\(nameS, tyS) ->
          case lookup nameS variantsT of
            Nothing -> False
            Just tyT -> subtype seen ctx tyS tyT
        ) variantsS
      (TySome name constrS tyS, TySome _ constrT tyT) ->
        isSubtype ctx constrS constrT &&
        isSubtype ctx tyS tyT &&
        isSubtype (addBind ctx (TyAliasBind name constrT)) tyS tyT
      (TyAll name constrS tyS, TyAll _ constrT tyT) ->
        isSubtype ctx constrS constrT &&
        isSubtype ctx tyS tyT &&
        isSubtype (addBind ctx (TyAliasBind name constrT)) tyS tyT
      _ -> False
    where
      markSeen = (tyS, tyT) : seen


typeof :: Ctx -> Tm -> Typechecked

typeof ctx TmTrue = Ok TyBool
typeof ctx TmFalse = Ok TyBool
typeof ctx TmUnit = Ok TyUnit
typeof ctx (TmInt _) = Ok TyInt

typeof ctx (TmBoundVar id _) =
  case getBind ctx id of
    Nothing -> Err $ NameNotFound ctx id
    Just (VarBind _ ty) -> Ok ty
    _ -> Err $ UntypeableBind $ indexToName ctx id

typeof ctx (TmAscription tm targetTy) = do
  tmTy <- typeof ctx tm
  if isSubtype ctx tmTy targetTy then Ok targetTy
  else Err $ IncompatibleAscription ctx tmTy targetTy

typeof ctx (TmTuple items) = TyTuple <$> traverse (typeof ctx) items

typeof ctx (TmRecord fields) =
  TyRecord <$> traverse (
    \(name, tm) -> do
      ty <- typeof ctx tm
      Ok (name, ty)
  ) fields

typeof ctx (TmLet binding def inTm) = do
  defTy <- typeof ctx def
  typeof (addBind ctx (VarBind binding defTy)) inTm

typeof ctx (TmFn binding argTy body) =
  TyFn argTy <$> typeof (addBind ctx (VarBind binding argTy)) body

typeof ctx (TmIf condTm thenTm elseTm) = do
  condTy <- typeof ctx condTm
  thenTy <- typeof ctx thenTm
  elseTy <- typeof ctx elseTm
  if not $ areEquivalent ctx condTy TyBool then Err $ ConditionNotBool ctx condTy
  else if areEquivalent ctx thenTy elseTy then Ok thenTy
  else Err $ BranchesTypesDiffer ctx [thenTy, elseTy]

typeof ctx (TmApp funcTm argTm) = do
  funcTy <- typeof ctx funcTm
  argTy <- typeof ctx argTm
  case simplifyTy ctx funcTy of
    TyFn fromTy toTy ->
      if isSubtype ctx argTy fromTy then Ok toTy
      else Err $ NotApplicable ctx argTy funcTy
    _ -> Err $ NotCallable ctx funcTy

typeof ctx (TmCase tm cases) = do
  ty <- typeof ctx tm
  case simplifyTy ctx ty of
    TyVariants variants ->
      do
        assertCasesExhausted
        returnTy <- assertAllBranchesHaveSameType
        Ok returnTy
      where
        assertCasesExhausted =
          if any (\(MatchAll _) -> True) cases then Ok () else
          case nonConveredTags of
            [] -> Ok ()
            _ -> Err $ VariantsNotExhausted nonConveredTags
          where
            onlyTagsCases = filter (
              \branch -> case branch of
                MatchTag _ _ _ -> True
                _ -> False
              ) cases
            casesTags = map (\(MatchTag tag _ _) -> tag) onlyTagsCases
            nonConveredTags = filter (\tag -> tag `notElem` casesTags) (map fst variants)
        assertAllBranchesHaveSameType =
          do
            casesTypes <- traverseCases
            let (headType : tailTypes) = casesTypes
            if all (areEquivalent ctx headType) tailTypes then Ok headType
            else Err $ BranchesTypesDiffer ctx casesTypes
          where
            traverseCases = sequence $ (map (
              \branch ->
                case branch of
                  MatchAll tm -> simplifyTy ctx <$> typeof ctx tm
                  MatchTag tag name tm ->
                    case lookup tag variants of
                      Nothing -> Err $ UnknownCaseTag tag
                      Just variantTy ->
                        tyShift (-1) <$> (typeof (addBind ctx (VarBind name variantTy)) tm)
              ) cases)
    _ -> Err $ NotAVariantType ctx ty

typeof ctx (TmTagged tag tm ty) = do
  tmTy <- typeof ctx tm
  case simplifyTy ctx ty of
    TyVariants variants ->
      case lookup tag variants of
        Just expectedTy ->
          if areEquivalent ctx expectedTy tmTy then Ok ty
          else Err $ VariantNotMatched ctx tag tmTy expectedTy
        _ -> Err $ VariantNotFound tag
    _ -> Err $ NotAVariantType ctx ty

typeof ctx (TmRecordProj tm field) = do
  ty <- typeof ctx tm
  case simplifyTy ctx ty of
    (TyRecord pairs) ->
      case lookup field pairs of
        Just itemTy -> Ok itemTy
        Nothing -> Err $ UnknownRecordProj ctx field ty
    _ -> Err $ UnknownRecordProj ctx field ty

typeof ctx (TmTupleProj tm index) = do
  ty <- typeof ctx tm
  case simplifyTy ctx ty of
    (TyTuple items) ->
      if length items <= index then Err $ UnknownTupleProj ctx index ty
      else Ok $ items !! index
    _ -> Err $ UnknownTupleProj ctx index ty

typeof ctx (TmPack actualTy implTm existentialTy@(TySome name constrTy quantifiedTy)) =
  if isSubtype ctx actualTy constrTy then do
    implTy <- typeof ctx implTm
    -- substitute exposed abstract type X for actual hidden type
    let packedTy = tySubstTop actualTy quantifiedTy
    -- impl should be applicable to declared type
    if isSubtype ctx implTy packedTy then Ok existentialTy
    else Err $ NotApplicable ctx packedTy implTy
  else Err $ ConstraintNotMatched ctx actualTy constrTy
typeof ctx (TmPack _ _ ty) = Err $ ExpectedExistential ctx ty

typeof ctx (TmUnpack abstractName termName targetTm bodyTm) = do
  targetTy <- typeof ctx targetTm
  case targetTy of
    TySome name constrTy reprTy -> do
      -- Bind a constrained type alias to abstract type name
      let ctx' = addBind ctx (TyConstrainedBind abstractName constrTy)
      -- Bind name to unpacked term
      let ctx'' = addBind ctx' (VarBind termName reprTy)
      -- Remove ealier bound type and term name
      tyShift (-2) <$> typeof ctx'' bodyTm
    _ -> Err $ ExpectedExistential ctx targetTy

typeof ctx (TmForAll tyName constrTy tm) = do
  -- Bind a constrained type alias to name in forall
  let ctx' = addBind ctx (TyConstrainedBind tyName constrTy)
  quantifiedTy <- typeof ctx' tm
  Ok $ TyAll tyName constrTy quantifiedTy

typeof ctx (TmConcretised tmWithUniversalTy concreteTy) = do
  universalTy <- typeof ctx tmWithUniversalTy
  case simplifyTy ctx universalTy of
    TyAll _ constrTy quantifiedTy ->
      if isSubtype ctx concreteTy constrTy then Ok $ tySubstTop concreteTy quantifiedTy
      else Err $ ConstraintNotMatched ctx concreteTy constrTy
    _ -> Err $ ExpectedUniveral ctx universalTy
