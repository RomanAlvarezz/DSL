module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (parse)

import Parser (pProgram)
import AST

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> runFile file
    _      -> printUsage

runFile :: FilePath -> IO ()
runFile file = do
  input <- readFile file
  case parse pProgram file input of
    Left err -> do
      putStrLn "❌ Error de parsing:"
      print err
      exitFailure
    Right ast -> do
      putStrLn "✅ Parsing exitoso. AST generado:\n"
      print ast

printUsage :: IO ()
printUsage = do
  putStrLn "JsonDB-DSL Parser"
  putStrLn "Uso:"
  putStrLn "  runhaskell Main.hs archivo.lis"
  putStrLn "  ó"
  putStrLn "  ghci Main.hs"
  putStrLn "  > main [\"archivo.lis\"]"

