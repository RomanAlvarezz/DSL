module JSONAdapter
  ( jsonToDatabase
  , databaseToJson
  , jsonToValue
  , valueToJson
  ) where

import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Scientific (floatingOrInteger)

import Evaluator

-------------------------------------------------------
-- JSON -> VALUE
-------------------------------------------------------

jsonToValue :: A.Value -> Value
jsonToValue (A.String t) =
  VString (T.unpack t)

jsonToValue (A.Bool b) =
  VBool b

jsonToValue A.Null =
  VNull

jsonToValue (A.Number n) =
  case floatingOrInteger n of
    Left f  -> VFloat f
    Right i -> VInt i

jsonToValue (A.Array arr) =
  VArray (map jsonToValue (V.toList arr))

jsonToValue (A.Object obj) =
  VObject
    [ (T.unpack k, jsonToValue v)
    | (k,v) <- HM.toList obj
    ]

-------------------------------------------------------
-- VALUE -> JSON
-------------------------------------------------------

valueToJson :: Value -> A.Value

valueToJson (VString s) =
  A.String (T.pack s)

valueToJson (VBool b) =
  A.Bool b

valueToJson VNull =
  A.Null

valueToJson (VInt i) =
  A.Number (fromIntegral i)

valueToJson (VFloat f) =
  A.Number (realToFrac f)

valueToJson (VArray xs) =
  A.Array (V.fromList (map valueToJson xs))

valueToJson (VObject fields) =
  A.Object $
    HM.fromList
      [ (T.pack k, valueToJson v)
      | (k,v) <- fields
      ]

-------------------------------------------------------
-- JSON -> DATABASE
-------------------------------------------------------

jsonToDatabase :: A.Value -> Either String Database

jsonToDatabase (A.Object obj) =
  Right $
    M.fromList
      [ (T.unpack collName, parseCollection val)
      | (collName, val) <- HM.toList obj
      ]

jsonToDatabase _ =
  Left "El JSON de la base debe ser un objeto"

parseCollection :: A.Value -> [Document]

parseCollection (A.Array arr) =
  map parseDocument (V.toList arr)

parseCollection _ =
  error "Una colección debe ser un array de documentos"

parseDocument :: A.Value -> Document

parseDocument (A.Object obj) =
  [ (T.unpack k, jsonToValue v)
  | (k,v) <- HM.toList obj
  ]

parseDocument _ =
  error "Un documento debe ser un objeto JSON"

-------------------------------------------------------
-- DATABASE -> JSON
-------------------------------------------------------

databaseToJson :: Database -> A.Value

databaseToJson db =
  A.Object $
    HM.fromList
      [ (T.pack coll, collectionToJson docs)
      | (coll, docs) <- M.toList db
      ]

collectionToJson :: [Document] -> A.Value

collectionToJson docs =
  A.Array $
    V.fromList (map documentToJson docs)

documentToJson :: Document -> A.Value

documentToJson doc =
  A.Object $
    HM.fromList
      [ (T.pack k, valueToJson v)
      | (k,v) <- doc
      ]
