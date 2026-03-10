module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Text.ParserCombinators.Parsec (parse)

import Parser (pProgram)
import AST
import Evaluator

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

main :: IO ()
main = do
  args <- getArgs
  case args of
    [queryFile, jsonFile] -> runFiles queryFile jsonFile
    _ -> printUsage

runFiles :: FilePath -> FilePath -> IO ()
runFiles queryFile jsonFile = do
  input <- readFile queryFile

  case parse pProgram queryFile input of
    Left err -> do
      putStrLn "❌ Error de parsing:"
      print err
      exitFailure

    Right ast -> do
      putStrLn "✅ Parsing exitoso"
      runEvaluator ast jsonFile


runEvaluator :: Program -> FilePath -> IO ()
runEvaluator ast jsonFile = do

  -- por ahora base vacía
  let initialDB = M.empty

  let runtimeCtx = RuntimeContext
        { views = M.empty
        , timestamps = M.empty
        }

  let initialState = EvalState
        { database = initialDB
        , runtime = runtimeCtx
        }

  result <- runExceptT (execStateT (evalProgram ast) initialState)

  case result of
    Left err ->
      putStrLn ("❌ Error de ejecución: " ++ show err)

    Right finalState -> do
      putStrLn "✅ Ejecución finalizada"
      print (database finalState)


printUsage :: IO ()
printUsage = do
  putStrLn "JsonDB-DSL"
  putStrLn "Uso:"
  putStrLn " runhaskell Main.hs consulta.lis database.json"