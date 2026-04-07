module Main where

-- Thanks, Gemini 3.1 Pro, for your excellent examples for testing :)

import Lambda.LamDef
import Lambda.LamResults
import Lambda.LamTypecheck
import Lambda.LamPrint


-- Helper to run a test and print the result nicely
runTest :: String -> Tm -> IO ()
runTest name tm = do
  putStrLn $ "--- Test: " ++ name ++ " ---"
  putStrLn $ printTm [] tm
  case typeof [] tm of
    Ok ty -> putStrLn $ "✅ OK: " ++ printTy [] ty
    Err err -> putStrLn $ "❌ ERROR: " ++ show err
  putStrLn ""

main :: IO ()
main = do

  -- 1. Basic Primitives
  runTest "Primitive Int" $
    TmInt 42

  runTest "Primitive If" $
    TmIf TmTrue (TmInt 1) (TmInt 2)

  -- 2. Records and Tuples
  runTest "Record Projection" $
    TmRecordProj (TmRecord [("x", TmInt 10), ("y", TmFalse)]) "y"

  runTest "Tuple Projection" $
    TmTupleProj (TmTuple [TmTrue, TmInt 99]) 1

  -- 3. Functions and Application
  let idFn = TmFn "x" TyInt (TmBoundVar 0 1) -- \x:Int. x
  runTest "Identity Function" idFn

  runTest "Function Application" $
    TmApp idFn (TmInt 42)

  -- 4. Let Bindings
  runTest "Let Binding" $
    TmLet "x" (TmInt 5) (TmBoundVar 0 1) -- let x = 5 in x

  -- ----------------------------------------------------------------------
  -- 5. RECURSIVE TYPES (The Ultimate Test)
  -- ----------------------------------------------------------------------

  -- Defining a List of Ints:
  -- List = μX. <nil: Unit, cons: {Int, X}>
  let tyIntList = TyRec "List" $
        TyVariants [
          ("nil", TyUnit),
          ("cons", TyTuple [TyInt, TyBoundVar 0 1]) -- 0 is index, 1 is ctxLen
        ]

  -- <nil=unit> as List
  let tmNil = TmTagged "nil" TmUnit tyIntList
  runTest "Recursive Type: Nil Tag" tmNil

  -- <cons={42, <nil=unit> as List}> as List
  let tmCons = TmTagged "cons" (TmTuple [TmInt 42, tmNil]) tyIntList
  runTest "Recursive Type: Cons Tag" tmCons

  -- Function that takes a List and returns a List
  -- \lst: List. lst
  runTest "Recursive Type: Identity Function" $
    TmApp (TmFn "lst" tyIntList (TmBoundVar 0 1)) tmCons

  -- Equirecursive looping test!
  -- Compares two structurally identical types that are unrolled differently.
  -- This will freeze in an infinite loop if your `areEquivalent` `seen` list doesn't work.
  let tyListUnrolledOnce = TyVariants [
          ("nil", TyUnit),
          ("cons", TyTuple [TyInt, tyIntList])
        ]
  putStrLn "--- Test: Equirecursive Infinity Trap ---"
  if areEquivalent [] tyIntList tyListUnrolledOnce
    then putStrLn "✅ OK: Equirecursive equivalence works!"
    else putStrLn "❌ ERROR: Types should be equivalent!"
  putStrLn ""

  -- 6. Deliberate Errors (to test typechecker rejections)
  runTest "ERROR: Bad If Condition" $
    TmIf (TmInt 1) (TmInt 1) (TmInt 2)

  runTest "ERROR: Bad Application" $
    TmApp idFn TmTrue

  let tyPoint2D = TyRecord [("x", TyInt), ("y", TyInt)]
  let tyPoint3D = TyRecord [("x", TyInt), ("y", TyInt), ("z", TyInt)]

  let tyList2D = TyRec "List2D" $
        TyVariants [
          ("nil", TyUnit),
          ("cons", TyTuple [tyPoint2D, TyBoundVar 0 1])
        ]

  let tyList3D = TyRec "List3D" $
        TyVariants [
          ("nil", TyUnit),
          ("cons", TyTuple [tyPoint3D, TyBoundVar 0 1])
        ]

  putStrLn "--- Test: Recursive Record Subtyping ---"
  if isSubtype [] tyList3D tyList2D
    then putStrLn "✅ OK: List of 3D Points is a subtype of List of 2D Points!"
    else putStrLn "❌ ERROR: Subtyping failed!"

  -------------------------------------------------------------------
  -- 1. UNIVERSAL TYPES (System F Generics)
  -------------------------------------------------------------------

  -- Polymorphic Identity Function: ΛX <: Top. \x: X. x
  let polyId = TmForAll "X" TyTop
                 (TmFn "x" (TyBoundVar 0 1) (TmBoundVar 0 2))

  runTest "Polymorphic Identity Type" polyId
  -- Should be: All X <: Top. (X -> X)

  -- Concretise (Apply) Identity to Int: (ΛX <: Top. \x: X. x) [Int]
  let appliedId = TmConcretised polyId TyInt

  runTest "Concretised Identity" appliedId
  -- Should be: Int -> Int

  -------------------------------------------------------------------
  -- 2. EXISTENTIAL TYPES (Modules)
  -------------------------------------------------------------------

  -- Counter Interface (Signature)
  -- CounterSig = Some State <: Top. { state: State, get: State -> Int }
  let counterSig = TySome "State" TyTop $
        TyRecord [
          ("state", TyBoundVar 0 1),
          ("get", TyFn (TyBoundVar 0 1) TyInt)
        ]

  -- Counter Implementation (using Int as the hidden state)
  -- impl = { state = 0, get = \x:Int. x }
  let counterImpl = TmRecord [
          ("state", TmInt 0),
          ("get", TmFn "x" TyInt (TmBoundVar 0 1))
        ]

  -- Pack the implementation into the Signature
  let myCounterModule = TmPack TyInt counterImpl counterSig

  runTest "Pack Counter Module" myCounterModule
  -- Should successfully return the TySome signature

  -- Unpacking and using the module:
  -- unpack X, ops = myCounterModule in ops.get(ops.state)
  let useCounter = TmUnpack "X" "ops" myCounterModule $
        TmApp
          (TmRecordProj (TmBoundVar 0 2) "get")   -- ops.get
          (TmRecordProj (TmBoundVar 0 2) "state") -- ops.state

  runTest "Unpack and Use Module" useCounter
  -- Should successfully return TyInt!
