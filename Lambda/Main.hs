module Lambda.Main where

import System.IO
import Lambda.LamParser
import Lambda.LamCompiler
import Lambda.LamRuntime

runFile :: FilePath -> IO ()
runFile filename = do
    hSetEncoding stdout utf8
    ast <- parseFile filename pExpr
    case ast of
        Nothing -> putStrLn "чё ты подсунул"
        Just expr -> case typeof [] (compile [] expr) of
          Left err -> putStrLn $ "Type check error: " ++ show err
          Right ty -> putStrLn $ "Revealed type is: " ++ show ty

evalStr code = do
    (_, ast) <- runParser pExpr code
    Just $ compile [] ast

main = undefined