module Main where

import Lambda.LamDef
import Lambda.LamResults
import Lambda.LamTypecheck
import Lambda.LamParserDef
import Lambda.LamGrammar
import Lambda.LamCompiler
import Lambda.LamPrint
import Lambda.LamEval
import System.IO
import Text.Parsec (putState)


showErr filename (ParserError errLine errCol errTy) =
  "Error in " ++ filename ++ " at line " ++ show errLine ++ " col " ++ show errCol ++ ":\n" ++
  show errTy

runFile :: FilePath -> IO ()
runFile filename = do
    hSetEncoding stdout utf8
    ast <- parseFile filename pExpr
    case ast of
        Err err -> putStrLn $ showErr filename err
        Ok expr -> case compile [] expr of
          Err err -> putStrLn $ show err
          Ok tm -> do
            case typeof [] tm of
              Err err -> putStrLn $ "Type check error: " ++ show err
              Ok ty -> do
                putStrLn $ "Revealed type is: " ++ printTy [] ty
                case eval [] tm of
                  Err err -> putStrLn $ "Runtime error, possibly I flicked up: " ++ show err
                  Ok tm' -> do
                    putStrLn $ "Result: " ++ printTm [] tm'

main = undefined
