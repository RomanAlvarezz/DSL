{-# LANGUAGE OverloadedStrings #-}

module JSONAdapter( jsonToDatabase, databaseToJson, jsonToValue, valueToJson) where
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Scientific (floatingOrInteger, Scientific, scientific)

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
    [ (T.unpack (K.toText k), jsonToValue v)
    | (k,v) <- KM.toList obj
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

--valueToJson (VFloat f) =
--  A.Number (realToFrac f)
valueToJson (VFloat f) =
  A.Number (truncateTOScientific 3 f)

valueToJson (VArray xs) =
  A.Array (V.fromList (map valueToJson xs))

valueToJson (VObject fields) =
  let pairs =
        [ (K.fromText (T.pack k), valueToJSON v)
        | (k,v) <- fields
        ]
  in A.Object (KM.fromList pairs)


-------------------------------------------------------
-- JSON -> DATABASE
-------------------------------------------------------
jsonToDatabase :: A.Value -> Either String (Database, Int)

jsonToDatabase (A.Object obj) =
  let

    metaVal = KM.lookup (K.fromText "_meta") obj

    nextId =
      case metaVal of
        Just (A.Object m) ->
          case KM.lookup (K.fromText "nextId") m of
            Just (A.Number n) ->
              case floatingOrInteger n of
                Right i -> i
                _ -> 1
            _ -> 1
        _ -> 1

    collections =
      KM.toList (KM.delete (K.fromText "_meta") obj)

    db =
      M.fromList
        [ (T.unpack (K.toText collName), parseCollection val)
        | (collName, val) <- collections
        ]

  in Right (db, nextId)

jsonToDatabase _ =
  Left "El JSON de la base debe ser un objeto"

parseCollection :: A.Value -> [Document]

parseCollection (A.Array arr) =
  map parseDocument (V.toList arr)

parseCollection _ =
  error "Una colección debe ser un array"

parseDocument :: A.Value -> Document

parseDocument (A.Object obj) =
  [ (T.unpack (K.toText k), jsonToValue v)
  | (k,v) <- KM.toList obj
  ]

parseDocument _ =
  error "Un documento debe ser un objeto"

-------------------------------------------------------
-- DATABASE -> JSON
-------------------------------------------------------
databaseToJson :: Database -> Int -> A.Value

databaseToJson db nextId =
  A.Object (
    KM.fromList (
      [ (K.fromText "_meta", metaObject nextId) ] ++
      [ (K.fromText (T.pack coll), collectionToJson docs)
      | (coll, docs) <- M.toList db
      ]
    )
  )

metaObject :: Int -> A.Value

metaObject n =
  A.Object (
    KM.fromList
      [ (K.fromText "nextId", A.Number (fromIntegral n)) ]
  )


collectionToJson :: [Document] -> A.Value

collectionToJson docs =
  A.Array (
    V.fromList (map documentToJson docs)
  )

documentToJson :: Document -> A.Value

documentToJson doc =
  A.Object (
    KM.fromList
      [ (K.fromText (T.pack k), valueToJson v)
      | (k,v) <- doc
      ]
  )

truncateTOScientific :: Int -> Double -> Scientific
truncateTOScientific n x =
  let factor = 10 ^ n
      scaled = truncate (x * fromIntegral factor)
  in scientific scaled (negate n)
