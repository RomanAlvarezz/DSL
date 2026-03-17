module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

import Text.ParserCombinators.Parsec (parse)

import Parser (pProgram)
import AST
import Evaluator
import JSONAdapter

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  putStrLn "DEBUG: inicio main"
  args <- getArgs
  case args of
    [queryFile, jsonFile] -> runFiles queryFile jsonFile
    _ -> printUsage


runFiles :: FilePath -> FilePath -> IO ()
runFiles queryFile jsonFile = do
  putStrLn "DEBUG: leyendo consulta"

  input <- fmap (filter (/= '\r')) (readFile queryFile)

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

  -------------------------------------------------------
  -- 1 LEER BASE DE DATOS JSON
  -------------------------------------------------------

  jsonBytes <- BL.readFile jsonFile

  jsonValue <- case A.decode jsonBytes of
    Nothing -> do
      putStrLn "❌ Error: JSON inválido"
      exitFailure
    Just v -> return v

  initialDB <- case jsonToDatabase jsonValue of
    Left err -> do
      putStrLn ("❌ Error en JSON de base: " ++ err)
      exitFailure
    Right db -> return db


  -------------------------------------------------------
  -- 2 CREAR ESTADO INICIAL DEL EVALUADOR
  -------------------------------------------------------

  let runtimeCtx = RuntimeContext
        { views = M.empty
        , timestamps = M.empty
        }

  let initialState = EvalState
        { database = initialDB
        , runtime = runtimeCtx
        }


  -------------------------------------------------------
  -- 3 EJECUTAR PROGRAMA
  -------------------------------------------------------

  result <- runExceptT (execStateT (evalProgram ast) initialState)

  case result of

    Left err ->
      putStrLn ("❌ Error de ejecución: " ++ show err)

    Right finalState -> do
      putStrLn "✅ Ejecución finalizada"

      -------------------------------------------------------
      -- 4 GUARDAR BASE DE DATOS FINAL
      -------------------------------------------------------

      let finalDB = database finalState

      let jsonOut = databaseToJson finalDB

      BL.writeFile jsonFile (AP.encodePretty jsonOut)

      putStrLn "💾 Base de datos actualizada"


printUsage :: IO ()
printUsage = do
  putStrLn "JsonDB-DSL"
  putStrLn "Uso:"
  putStrLn " runhaskell Main.hs consulta.lis database.json"

{--module Main where

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
  putStrLn "DEBUG: inicio main"
  args <- getArgs
  case args of
    [queryFile, jsonFile] -> runFiles queryFile jsonFile
    _ -> printUsage

runFiles :: FilePath -> FilePath -> IO ()
runFiles queryFile jsonFile = do
  putStrLn "DEBUG: antes de readFile"
  --input <- readFile queryFile
  input <- fmap (filter (/= '\r')) (readFile queryFile)

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
--}
