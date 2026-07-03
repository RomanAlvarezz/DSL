module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec (parse)
import Parser (pProgram)
import AST
import Evaluator
import JSONAdapter
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8


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

  input <- readFile queryFile

  case parse pProgram queryFile input of
    Left err -> do
      putStrLn "❌ Error de parsing:"
      print err
      exitFailure

    Right ast -> do
      putStrLn "✅ Parsing exitoso"
      runEvaluator ast jsonFile


runEvaluator :: Comm -> FilePath -> IO ()
runEvaluator ast jsonFile = do

  -------------------------------------------------------
  -- 1 LEER BASE DE DATOS JSON
  -------------------------------------------------------

  jsonBytes <- BL.readFile jsonFile -- aca lee la base de datos
 
  jsonValue <- case A.decode jsonBytes of -- A.decode jsonBytes intenta interpretar esos bytes como JSON
    Nothing -> do
      putStrLn "❌ Error: JSON inválido"
      exitFailure
    Just v -> return v

  (initialDB, nextIdVal) <-
    case jsonToDatabase jsonValue of
      Left err -> do
        putStrLn ("❌ Error en JSON de base: " ++ err)
        exitFailure
      Right res -> return res

  -------------------------------------------------------
  -- 2 CREAR ESTADO INICIAL DEL EVALUADOR
  -------------------------------------------------------
  let initialState = EvalState
        { database = initialDB
        , nextId = nextIdVal
        , logs = (0,[])
        }


  -------------------------------------------------------
  -- 3 EJECUTAR PROGRAMA
  -------------------------------------------------------
  -- limpiar timestamps y views al inicio de cada ejecución
  BL.writeFile "timestamps.json" (A.encode (A.Object mempty))
  BL.writeFile "views.json" (A.encode (A.Object mempty))

  result <- runEval (evalComm ast) initialState

  -- este case seria la nueva forma de ver el resultado de evaluator
  case result of

    Left err ->
      putStrLn ("❌ Error de ejecución: " ++ showError err)

    Right (_, finalState) -> do
      putStrLn "✅ Ejecución finalizada"
      let (docsChanged, collsChanged) = logs finalState

      putStrLn ("📄 Documentos modificados: " ++ show docsChanged)
      putStrLn ("📦 Colecciones modificadas: " ++ show (length collsChanged))
      putStrLn ("📂 Nombres de colecciones modificadas: " ++ show collsChanged)

      -------------------------------------------------------
      -- 4 GUARDAR BASE DE DATOS FINAL
      -------------------------------------------------------

      let finalDB = database finalState
      let finalNextId = nextId finalState
      let jsonOut = databaseToJson finalDB finalNextId

      BL.writeFile jsonFile (AP.encodePretty jsonOut)


printUsage :: IO ()
printUsage = do
  putStrLn "JsonDB-DSL"
  putStrLn "Uso:"
  putStrLn " runhaskell Main.hs consulta.lis database.json"
