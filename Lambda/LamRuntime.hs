module Lambda.LamRuntime where

import Data.List (sort)

data Tm
  = TmTrue
  | TmFalse
  | TmInt Int
  | TmIf Tm Tm Tm
  | TmBoundVar Int Int
  | TmAbs String Ty Tm
  | TmApp Tm Tm
  | TmLet String Tm Tm -- name value and in block
  | TmUnit
  | TmPair Tm Tm
  | TmProj Int Tm -- t.i
  | TmTuple [Tm]
  | TmRecord [(String, Tm)]
  | TmField String Tm -- t.field
  deriving (Show)

data Ty
  = TyBool
  | TyInt
  | TyFn Ty Ty
  | TyUnknown
  | TyUnit
  | TyPair Ty Ty
  | TyTuple [Ty]
  | TyRecord [(String, Ty)]
  | TyTop
  deriving (Eq)

instance Show Ty where
  show TyBool = "Bool"
  show (TyFn a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show TyUnknown = "?"
  show TyUnit = "Unit"
  show (TyPair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"
  show (TyTuple items) = "(" ++ showItems items ++ ")"
    where
      showItems [] = ""
      showItems [item] = show item
      showItems (item : rest) = show item ++ ", " ++ showItems rest
  show (TyRecord pairs) = "{" ++ showPairs pairs ++ "}"
    where
      showPairs [] = ""
      showPairs [(name, ty)] = name ++ "=" ++ show ty
      showPairs ((name, ty) : rest) = name ++ "=" ++ show ty ++ ", " ++ showPairs rest
  show TyTop = "Top"
  show TyInt = "Int"

data TyError
  = TyInapplicable Ty Ty -- got expected
  | TyNotTheSame Ty Ty
  | NotTypeableBind
  | UnknownProj Int Ty
  | UnknownField String Ty

instance Show TyError where
  show (TyInapplicable x y) = "Inapplicable types: " ++ show x ++ " and " ++ show y
  show (TyNotTheSame x y) = "Types must be same: " ++ show x ++ " != " ++ show y
  show NotTypeableBind = "Not a typeable binding"
  show (UnknownProj n ty) = "Cannot project " ++ show n ++ " item of " ++ show ty
  show (UnknownField n ty) = "Cannot project " ++ show n ++ " field of " ++ show ty

type TyResult = Either TyError Ty

type Ctx = [Bind]

data Bind
  = NameBind String
  | VarBind String Ty
  deriving (Show)

addBind :: Ctx -> Bind -> Ctx
addBind ctx bind = bind : ctx

getBind :: Ctx -> Int -> Bind
getBind [] index = error "No such index"
getBind (x : _) 0 = x
getBind (_ : rest) index = getBind rest (index - 1)


typeFromCtx :: Ctx -> Int -> TyResult
typeFromCtx ctx index =
  case bind of
    (VarBind name ty) -> Right ty
    _ -> Left NotTypeableBind
  where
    bind = getBind ctx index

--
-- Typing rules
--

typeof :: Ctx -> Tm -> TyResult

typeof ctx TmTrue = Right TyBool

typeof ctx TmFalse = Right TyBool

typeof ctx (TmInt _) = Right TyInt

typeof ctx (TmIf t1 t2 t3) = do
  ty1 <- typeof ctx t1
  ty2 <- typeof ctx t2
  ty3 <- typeof ctx t3
  if (==) ty1 TyBool then
    if (==) ty2 ty3 then
      Right ty2   --  or ty3
    else
      Left $ TyNotTheSame ty2 ty3
  else
    Left $ TyInapplicable ty1 TyBool

typeof ctx (TmBoundVar index _) = typeFromCtx ctx index

typeof ctx (TmAbs argName argTy bodyTm) = do
  let ctx' = addBind ctx (VarBind argName argTy)
  bodyTy <- typeof ctx' bodyTm
  Right $ TyFn argTy bodyTy

typeof ctx (TmApp tm1 tm2) = do
  ty1 <- typeof ctx tm1
  ty2 <- typeof ctx tm2
  case ty1 of
    (TyFn a b) ->
      if ty2 `subtype` a then Right b
      else Left $ TyInapplicable ty2 a
    _ -> Left $ TyInapplicable ty1 (TyFn ty2 TyUnknown)

typeof ctx (TmLet bindName bindTm inTm) = do
  bindTy <- typeof ctx bindTm
  let ctx' = addBind ctx (VarBind bindName bindTy)
  typeof ctx' inTm

typeof ctx TmUnit = Right TyUnit

typeof ctx (TmPair tm1 tm2) = do
  ty1 <- typeof ctx tm1
  ty2 <- typeof ctx tm2
  Right $ TyPair ty1 ty2

typeof ctx (TmProj n tm) = do
  ty <- typeof ctx tm
  case ty of
    (TyPair a b) -> case n of
      0 -> Right a
      1 -> Right b
      _ -> Left $ UnknownProj n ty
    (TyTuple items) ->
      if length items <= n then Left $ UnknownProj n ty
      else Right $ items !! n
    _ -> Left $ UnknownProj n ty

typeof ctx (TmTuple items) =
  case traverse (typeof ctx) items of
    Right types -> Right $ TyTuple types
    Left err -> Left err

typeof ctx (TmRecord pairs) =
  case traverse figureOutPair pairs of
    Right typedPairs -> Right $ TyRecord typedPairs
    Left err -> Left err
  where
    figureOutPair (name, tm) = do
      ty <- typeof ctx tm
      Right (name, ty)

typeof ctx (TmField field tm) = do
  ty <- typeof ctx tm
  case ty of
    (TyRecord pairs) ->
      case lookup field pairs of
        Just itemTy -> Right itemTy
        Nothing -> Left $ UnknownField field ty
    _ -> Left $ UnknownField field ty

--
-- Subtyping rules
--

subtype :: Ty -> Ty -> Bool

subtype (TyRecord fieldsS) (TyRecord fieldsT) =
  all (\(tFieldName, tFieldTy) ->
    case lookup tFieldName fieldsS of
      Nothing -> False
      Just sFieldTy -> sFieldTy `subtype` tFieldTy
  ) fieldsT

subtype _ TyTop = True

subtype (TyFn tySFrom tySTo) (TyFn tyTFrom tyTTo) =
   tyTFrom `subtype` tySFrom && tySTo `subtype` tyTTo

subtype tyS tyT
  | tyS == tyT = True
  | otherwise = False



main = do
  let ty1 = typeof [] $ TmRecord [("x", TmTrue), ("y", TmFalse)]
  let ty2 = typeof [] $ TmRecord [("x", TmTrue)]
  case (ty1, ty2) of
    (Right ty1, Right ty2) ->
      print $ (ty1 `subtype` ty2)
    (Left err, _) ->
      print $ show err
    (_, Left err) ->
      print $ show err
