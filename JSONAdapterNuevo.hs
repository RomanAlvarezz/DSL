{-# LANGUAGE OverloadedStrings #-}

module JSONAdapterNuevo(jsonToDatabase, databaseToJson, jsonToValue, valueToJson,
  -- para snapshots
  databaseToJsonSnap, jsonToDatabaseSnap, timestampSnapshotToJson, jsonToTimestampSnapshot,
  -- para views
  findToJson, jsonToFind, parseCollection
) where
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Scientific (floatingOrInteger, Scientific, scientific)

import ASTNuevo
import Prelude hiding (Eq)
import ValueNuevo hiding (FieldName)

-------------------------------------------------------
-- JSON -> VALUE
-------------------------------------------------------
jsonToValue :: A.Value -> Value
jsonToValue (A.String t) =
  VString (str t)

jsonToValue (A.Bool b) =
  VBool b

jsonToValue A.Null =
  VNull
-- cambio aca por tipo nuestro Number
jsonToValue (A.Number n) =
  case floatingOrInteger n of
    Left f  -> VNum (NFloat f)
    Right i -> VNum (NInt i)

jsonToValue (A.Array arr) =
  VArray (map jsonToValue (V.toList arr))

jsonToValue (A.Object obj) =
  VObject
    [ (keyToString k, jsonToValue v)
    | (k,v) <- KM.toList obj
    ]

-------------------------------------------------------
-- VALUE -> JSON
-------------------------------------------------------
valueToJson :: Value -> A.Value
valueToJson (VString s) =
  A.String (text s)

valueToJson (VBool b) =
  A.Bool b

valueToJson VNull =
  A.Null
-- cambiado a nuestro tipo Number del AST
valueToJson (VNum (NInt i)) =
  A.Number (fromIntegral i)
-- cambiado a nuestro tipo Number del AST
valueToJson (VNum (NFloat f)) =
  A.Number (truncateTOScientific 5 f)

valueToJson (VArray xs) =
  A.Array (V.fromList (map valueToJson xs))

valueToJson (VObject fields) =
  let pairs =
        [ (stringToKey k, valueToJson v)
        | (k,v) <- fields
        ]
  in A.Object (KM.fromList pairs)


-------------------------------------------------------
-- JSON -> DATABASE
-------------------------------------------------------
jsonToDatabase :: A.Value -> Either String (Database, Int)
jsonToDatabase (A.Object obj) = do

  let metaVal = KM.lookup (stringToKey "_meta") obj

  let nextId =
        case metaVal of
          Just (A.Object m) ->
            case KM.lookup (stringToKey "nextId") m of
              Just (A.Number n) ->
                case floatingOrInteger n of
                  Right i -> i
                  _ -> 1
              _ -> 1
          _ -> 1

  let collections =
        KM.toList (KM.delete (stringToKey "_meta") obj)

  db <- mapM parsePair collections

  return (db, nextId)

  where
    parsePair (collName, val) = do
      docs <- parseCollection val
      return (keyToString collName, docs)

jsonToDatabase _ =
  Left "El JSON de la base debe ser un objeto"

parseDocument :: A.Value -> Either String Document
parseDocument (A.Object obj) =
  Right
    [ (keyToString k, jsonToValue v)
    | (k,v) <- KM.toList obj
    ]

parseDocument _ =
  Left "Un documento debe ser un objeto"


parseCollection :: A.Value -> Either String [Document]
parseCollection (A.Array arr) =
  mapM parseDocument (V.toList arr)

parseCollection _ =
  Left "Colección inválida: se esperaba un array"

-------------------------------------------------------
-- DATABASE -> JSON
-------------------------------------------------------
databaseToJson :: Database -> Int -> A.Value
databaseToJson db nextId =
  A.Object (
    KM.fromList (
      [ (stringToKey "_meta", metaObject nextId) ] ++
      [ (stringToKey coll, collectionToJson docs)
      | (coll, docs) <- db
      ]
    )
  )

metaObject :: Int -> A.Value
metaObject n =
  A.Object (
    KM.fromList
      [ (stringToKey "nextId", A.Number (fromIntegral n)) ]
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
      [ (stringToKey k, valueToJson v)
      | (k,v) <- doc
      ]
  )

truncateTOScientific :: Int -> Double -> Scientific
truncateTOScientific n x =
  let factor = 10 ^ n
      scaled = truncate (x * fromIntegral factor)
  in scientific scaled (negate n)

-------------------------------------------------------
-- NumExp -> JSON
-------------------------------------------------------
numExpToJson :: NumExp -> A.Value
numExpToJson (NConst (NInt n)) =
  obj "int" [("value", A.Number (fromIntegral n))]

numExpToJson (NConst (NFloat f)) =
  obj "float" [("value", A.Number (realToFrac f))]

{--
numExpToJson (NVar f) =
  obj "var" [("name", A.String (text f))]
--}

-- nuevo
numExpToJson (NPath p) =
  obj "path"
    [ ("value", pathExpToJson p) ]

numExpToJson (NAdd a b) =
  obj "add"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

numExpToJson (NSub a b) =
  obj "sub"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

numExpToJson (NMul a b) =
  obj "mul"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

numExpToJson (NDiv a b) =
  obj "div"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]


-------------------------------------------------------
-- JSON -> NumExp
-------------------------------------------------------
jsonToNumExp :: A.Value -> Either String NumExp
jsonToNumExp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of

    Just (A.String "int") -> do
      v <- getIntValue
      Right (NConst (NInt v))

    Just (A.String "float") -> do
      v <- getFloatValue
      Right (NConst (NFloat v))
    {--
    Just (A.String "var") -> do
      name <- getStringField "name" obj
      Right (NVar name)
    --}
    -- nuevo
    Just (A.String "path") -> do
      v <- getField "value" obj
      p <- jsonToPathExp v
      Right (NPath p)


    Just (A.String "add") ->
      bin NAdd

    Just (A.String "sub") ->
      bin NSub

    Just (A.String "mul") ->
      bin NMul

    Just (A.String "div") ->
      bin NDiv

    _ ->
      Left "NumExp desconocida"

  where

    getIntValue = do
      v <- getField "value" obj
      case v of
        A.Number n ->
          case floatingOrInteger n of
            Right i -> Right i
            _ -> Left "No es int"
        _ ->
          Left "No es numero"

    getFloatValue = do
      v <- getField "value" obj
      case v of
        A.Number n ->
          case floatingOrInteger n of
            Left f  -> Right f
            Right i -> Right (fromIntegral i)
        _ ->
          Left "No es numero"

    bin cons = do
      l <- getField "l" obj
      r <- getField "r" obj
      left <- jsonToNumExp l
      right <- jsonToNumExp r
      return (cons left right)

jsonToNumExp _ =
  Left "NumExp invalida"

-------------------------------------------------------
-- StrExp -> JSON
-------------------------------------------------------
strExpToJson :: StrExp -> A.Value
strExpToJson (SConst s) =
  obj "string"
    [ ("value", A.String (text s)) ]

  {- 
  strExpToJson (SVar f) =
  obj "var"
    [ ("name", A.String (text f)) ] 
  -}
-- nuevo
strExpToJson (SPath p) =
  obj "path"
    [ ("value", pathExpToJson p) ]

-------------------------------------------------------
-- JSON -> StrExp
-------------------------------------------------------
jsonToStrExp :: A.Value -> Either String StrExp
jsonToStrExp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of

    Just (A.String "string") -> do
      v <- getStringValue
      Right (SConst v)

    {--
    Just (A.String "var") -> do
      name <- getStringField "name" obj
      Right (SVar name)
    --}
    -- nuevo
    Just (A.String "path") -> do
      v <- getField "value" obj
      p <- jsonToPathExp v
      Right (SPath p)

    _ ->
      Left "StrExp desconocida"

  where

    getStringValue = do
      v <- getField "value" obj
      case v of
        A.String t -> Right (str t)
        _ -> Left "No es string"

jsonToStrExp _ =
  Left "StrExp invalida"

-------------------------------------------------------
-- BoolExp -> JSON
-------------------------------------------------------
boolExpToJson :: BoolExp -> A.Value
boolExpToJson BTrue =
  obj "true" []

boolExpToJson BFalse =
  obj "false" []

{--
boolExpToJson (BVar f) =
  obj "boolVar"
    [ ("name", A.String (text f)) ]
--}
-- nuevo
boolExpToJson (BPath p) =
  obj "boolPath"
    [ ("value", pathExpToJson p) ]

boolExpToJson (Not b) =
  obj "not"
    [ ("value", boolExpToJson b) ]

boolExpToJson (And a b) =
  obj "and"
    [ ("l", boolExpToJson a)
    , ("r", boolExpToJson b)
    ]

boolExpToJson (Or a b) =
  obj "or"
    [ ("l", boolExpToJson a)
    , ("r", boolExpToJson b)
    ]

boolExpToJson (EqNum a b) =
  obj "eqN"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

boolExpToJson (NeqNum a b) =
  obj "neqN"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

boolExpToJson (EqStr a b) =
  obj "eqS"
    [ ("l", strExpToJson a)
    , ("r", strExpToJson b)
    ]

boolExpToJson (NeqStr a b) =
  obj "neqS"
    [ ("l", strExpToJson a)
    , ("r", strExpToJson b)
    ]

boolExpToJson (EqBool a b) =
  obj "eqB"
    [ ("l", boolExpToJson a)
    , ("r", boolExpToJson b)
    ]

boolExpToJson (NeqBool a b) =
  obj "neqB"
    [ ("l", boolExpToJson a)
    , ("r", boolExpToJson b)
    ]

boolExpToJson (IsNull f) =
  obj "isNull"
    [ ("field", pathExpToJson f) ]

-- ==========================================================

boolExpToJson (Lt a b) =
  obj "lt"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

boolExpToJson (Le a b) =
  obj "le"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

boolExpToJson (Gt a b) =
  obj "gt"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

boolExpToJson (Ge a b) =
  obj "ge"
    [ ("l", numExpToJson a)
    , ("r", numExpToJson b)
    ]

boolExpToJson (Exists p) =
  obj "exists"
    [ ("value", pathExpToJson p) ]

pathExpToJson :: PathExp -> A.Value
pathExpToJson (PVar f) =
  obj "pathVar" [("name", A.String (text f))]

pathExpToJson (PAccess p f) =
  obj "pathAccess"
    [ ("path", pathExpToJson p)
    , ("field", A.String (text f))
    ]

-------------------------------------------------------
-- JSON -> BoolExp
-------------------------------------------------------
jsonToBoolExp :: A.Value -> Either String BoolExp
jsonToBoolExp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of

    Just (A.String "true") ->
      Right BTrue

    Just (A.String "false") ->
      Right BFalse

    {--
    Just (A.String "boolVar") -> do
      name <- getStringField "name" obj
      Right (BVar name)
    --}
    -- nuevo
    Just (A.String "boolPath") -> do
      v <- getField "value" obj
      p <- jsonToPathExp v
      Right (BPath p)

    Just (A.String "not") -> do
      v <- getField "value" obj
      b <- jsonToBoolExp v
      return (Not b)

    Just (A.String "and") -> do
      l <- getField "l" obj
      r <- getField "r" obj
      left <- jsonToBoolExp l
      right <- jsonToBoolExp r
      return (And left right)

    Just (A.String "or") -> do
      l <- getField "l" obj
      r <- getField "r" obj
      left <- jsonToBoolExp l
      right <- jsonToBoolExp r
      return (Or left right)

    Just (A.String "eqN") ->
      binNum EqNum

    Just (A.String "neqN") ->
      binNum NeqNum

    Just (A.String "eqS") ->
      binStr EqStr

    Just (A.String "neqS") ->
      binStr NeqStr

    Just (A.String "eqB") ->
      binBool EqBool

    Just (A.String "neqB") ->
      binBool NeqBool

    Just (A.String "isNull") -> do
      v <- getField "field" obj
      p <- jsonToPathExp v
      Right (IsNull p)

    Just (A.String "lt") ->
      binNum Lt

    Just (A.String "le") ->
      binNum Le

    Just (A.String "gt") ->
      binNum Gt

    Just (A.String "ge") ->
      binNum Ge

    Just (A.String "exists") -> do
      v <- getField "value" obj
      p <- jsonToPathExp v
      return (Exists p)

    _ ->
      Left "BoolExp desconocido"

  where

    binNum cons = do
      l <- getField "l" obj
      r <- getField "r" obj
      left <- jsonToNumExp l
      right <- jsonToNumExp r
      return (cons left right)

    binStr cons = do
      l <- getField "l" obj
      r <- getField "r" obj
      left <- jsonToStrExp l
      right <- jsonToStrExp r
      return (cons left right)

    binBool cons = do
      l <- getField "l" obj
      r <- getField "r" obj
      left <- jsonToBoolExp l
      right <- jsonToBoolExp r
      return (cons left right)

jsonToBoolExp _ =
  Left "BoolExp invalido"

jsonToPathExp :: A.Value -> Either String PathExp
jsonToPathExp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of
    Just (A.String "pathVar") -> do
      name <- getStringField "name" obj
      Right (PVar name)
    Just (A.String "pathAccess") -> do
      p <- getField "path" obj
      f <- getStringField "field" obj
      path <- jsonToPathExp p
      return (PAccess path f)
    _ ->
      Left "PathExp desconocido"

jsonToPathExp _ =
  Left "PathExp invalido"

-------------------------------------------------------
-- JsonExp -> JSON
-------------------------------------------------------
jsonExpToJson :: JsonExp -> A.Value
jsonExpToJson (JObject fields) =
  obj "object"
    [ ("fields",
        A.Array (V.fromList (map jsonFieldToJson fields)))
    ]

jsonExpToJson (JArray xs) =
  obj "array"
    [ ("values",
        A.Array (V.fromList (map jsonExpToJson xs)))
    ]

jsonExpToJson (JNum n) =
  obj "jsonNum"
    [ ("value", numExpToJson n) ]

jsonExpToJson (JStr s) =
  obj "jsonStr"
    [ ("value", strExpToJson s) ]

jsonExpToJson (JBool b) =
  obj "jsonBool"
    [ ("value", boolExpToJson b) ]

jsonExpToJson JNull =
  obj "jsonNull" []

jsonExpToJson (JPath p) =
  obj "jsonPath"
    [ ("value", pathExpToJson p) ]


-- helper para fields de JObject
jsonFieldToJson :: (FieldName, JsonExp) -> A.Value
jsonFieldToJson (k, v) =
  A.Object (KM.fromList
    [ (stringToKey "k", A.String (text k))
    , (stringToKey "v", jsonExpToJson v)
    ])

-------------------------------------------------------
-- JSON -> JsonExp
-------------------------------------------------------
jsonToJsonExp :: A.Value -> Either String JsonExp
jsonToJsonExp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of
    -- object
    Just (A.String "object") -> do
      arr <- getArrayField "fields" obj
      fields <- mapM jsonToJsonField arr
      Right (JObject fields)
    -- array
    Just (A.String "array") -> do
      arr <- getArrayField "values" obj
      xs <- mapM jsonToJsonExp arr
      Right (JArray xs)
    -- num
    Just (A.String "jsonNum") -> do
      v <- getField "value" obj
      n <- jsonToNumExp v
      return (JNum n)
    -- str
    Just (A.String "jsonStr") -> do
      v <- getField "value" obj
      s <- jsonToStrExp v
      return (JStr s)
    -- bool
    Just (A.String "jsonBool") -> do
      v <- getField "value" obj
      b <- jsonToBoolExp v
      return (JBool b)
    -- null
    Just (A.String "jsonNull") ->
      Right JNull
    -- path
    Just (A.String "jsonPath") -> do
      v <- getField "value" obj
      p <- jsonToPathExp v
      return (JPath p)

    _ ->
      Left "JsonExp desconocida"

jsonToJsonExp _ =
  Left "JsonExp invalida"


-- Helper para fields de JObject
jsonToJsonField :: A.Value -> Either String (FieldName, JsonExp)
jsonToJsonField (A.Object obj) = do
  k <- case KM.lookup (stringToKey "k") obj of
    Just (A.String t) -> Right (str t)
    _ -> Left "field key invalida"
  v <- case KM.lookup (stringToKey "v") obj of
    Just val -> jsonToJsonExp val
    Nothing -> Left "field sin valor"
  return (k, v)

jsonToJsonField _ =
  Left "field mal formado"

-------------------------------------------------------
-- Exp -> JSON
-------------------------------------------------------
{--
expToJson :: Exp -> A.Value
expToJson (ENum n) =
  obj "numExp"
    [("value", numExpToJson n)]

expToJson (EBool b) =
  obj "boolExp"
    [("value", boolExpToJson b)]

expToJson (EJson j) =
  obj "jsonExp"
    [("value", jsonExpToJson j)]

expToJson (EStr s) =
  obj "strExp"
    [("value", strExpToJson s)]
--}
-------------------------------------------------------
-- JSON -> Exp
-------------------------------------------------------
{--
jsonToExp :: A.Value -> Either String Exp
jsonToExp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of
    Just (A.String "numExp") -> do
      v <- getField "value" obj
      n <- jsonToNumExp v
      return (ENum n)
    Just (A.String "boolExp") -> do
      v <- getField "value" obj
      b <- jsonToBoolExp v
      return (EBool b)
    Just (A.String "jsonExp") -> do
      v <- getField "value" obj
      j <- jsonToJsonExp v
      return (EJson j)
    Just (A.String "strExp") -> do
      v <- getField "value" obj
      s <- jsonToStrExp v
      return (EStr s)
    _ ->
      Left "Exp desconocida"
jsonToExp _ =
  Left "Exp invalida"
--}


-------------------------------------------------------
-- database -> Json (sin nextId, para timestamps)
-------------------------------------------------------
databaseToJsonSnap :: Database -> A.Value
databaseToJsonSnap db =
  A.Object (
    KM.fromList
      [ (stringToKey coll, collectionToJson docs)
      | (coll, docs) <- db
      ]
  )

-------------------------------------------------------
-- Json -> database (sin nextId, para timestamps)
-------------------------------------------------------
jsonToDatabaseSnap :: A.Value -> Either String Database
jsonToDatabaseSnap (A.Object obj) = do
  pairs <- mapM parsePair (KM.toList obj)
  return pairs
  where
    parsePair (collName, val) = do
      docs <- parseCollection val
      return (keyToString collName, docs)

jsonToDatabaseSnap _ =
  Left "El snapshot de la base debe ser un objeto"


-------------------------------------------------------
-- timestamp -> Json
-------------------------------------------------------
timestampSnapshotToJson :: TimestampSnapshot -> A.Value
timestampSnapshotToJson (DBSnapshot db) =
  A.Object (KM.fromList
    [ (stringToKey "type", A.String "db")
    , (stringToKey "data", databaseToJsonSnap db)
    ])

timestampSnapshotToJson (CollSnapshot coll docs) =
  A.Object (KM.fromList
    [ (stringToKey "type", A.String "collection")
    , (stringToKey "collection", A.String (text coll))
    , (stringToKey "data", collectionToJson docs)
    ])


-------------------------------------------------------
-- Json -> timestamp
-------------------------------------------------------
jsonToTimestampSnapshot :: A.Value -> Either String TimestampSnapshot
jsonToTimestampSnapshot (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of

    Just (A.String "db") ->
      case KM.lookup (stringToKey "data") obj of
        Just dbVal -> do
          db <- jsonToDatabaseSnap dbVal
          return (DBSnapshot db)
        Nothing ->
          Left "Falta campo 'data' en snapshot db"
    Just (A.String "collection") ->
      case ( KM.lookup (stringToKey "collection") obj
           , KM.lookup (stringToKey "data") obj ) of
        (Just (A.String coll), Just docsVal) -> do
          docs <- parseCollection docsVal
          return (CollSnapshot (str coll) docs)
        _ ->
          Left "Snapshot collection mal formado"
    _ ->
      Left "Tipo de snapshot desconocido"
jsonToTimestampSnapshot _ =
  Left "Snapshot debe ser un objeto JSON"

-------------------------------------------------------
-- FIND -> JSON
-------------------------------------------------------
findToJson :: Find -> A.Value
findToJson (Find coll ops term) =
  A.Object (KM.fromList
    [ (stringToKey "collection", A.String (text coll))
    , (stringToKey "pipeline", A.Array (V.fromList (map queryOpToJson ops)))
    , (stringToKey "terminal", terminalToJson term)
    ])

-------------------------------------------------------
-- JSON -> Find
-------------------------------------------------------
jsonToFind :: A.Value -> Either String Find
jsonToFind (A.Object obj) = do
  coll <- getStringField "collection" obj

  arr <- getArrayField "pipeline" obj
  ops <- mapM jsonToQueryOp arr

  termVal <- getField "terminal" obj
  term <- jsonToTerminal termVal

  return (Find coll ops term)

jsonToFind _ = Left "Find debe ser objeto"

-------------------------------------------------------
-- QueryOp -> JSON
-------------------------------------------------------
queryOpToJson :: QueryOp -> A.Value
queryOpToJson (QFilter cond) =
  obj "filter" [("cond", boolExpToJson cond)]

queryOpToJson (QSelect fields) =
  obj "select" [("fields", A.Array (V.fromList (map (A.String . text) fields)))]

queryOpToJson (QLimit n) =
  obj "limit" [("value", A.Number (fromIntegral n))]

queryOpToJson (QSort fields) =
  obj "sort"
    [ ("fields", A.Array (V.fromList (map sortFieldToJson fields))) ]

queryOpToJson (QGroup gs) =
  obj "group" [("spec", groupSpecToJson gs)]

-------------------------------------------------------
-- JSON -> QueryOp
-------------------------------------------------------
jsonToQueryOp :: A.Value -> Either String QueryOp
jsonToQueryOp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of

    Just (A.String "filter") -> do
      condVal <- getField "cond" obj
      cond <- jsonToBoolExp condVal
      return (QFilter cond)

    Just (A.String "select") -> do
      arr <- getArrayField "fields" obj
      fields <- mapM expectString arr
      return (QSelect fields)

    Just (A.String "limit") -> do
      n <- getIntField "value" obj
      return (QLimit n)

    Just (A.String "sort") -> do
      arr <- getArrayField "fields" obj
      fs <- mapM jsonToSortField arr
      return (QSort fs)

    Just (A.String "group") -> do
      specVal <- getField "spec" obj
      spec <- jsonToGroupSpec specVal
      return (QGroup spec)

    _ -> Left "QueryOp desconocido"

  where
    expectString (A.String t) = Right (str t)
    expectString _ = Left "select: field no es string"

jsonToQueryOp _ = Left "QueryOp debe ser objeto"


-------------------------------------------------------
-- Sort -> JSON
-------------------------------------------------------
sortFieldToJson :: (FieldName, SortOrder) -> A.Value
sortFieldToJson (f, ord) =
  A.Object (KM.fromList
    [ (stringToKey "field", A.String (text f))
    , (stringToKey "order", A.String (text (show ord)))
    ])

-------------------------------------------------------
-- JSON -> Sort
-------------------------------------------------------
jsonToSortField :: A.Value -> Either String (FieldName, SortOrder)
jsonToSortField (A.Object obj) = do
  f <- case KM.lookup (stringToKey "field") obj of
    Just (A.String t) -> Right (str t)
    _ -> Left "sort field invalido"

  ord <- case KM.lookup (stringToKey "order") obj of
    Just (A.String "Asc") -> Right Asc
    Just (A.String "Desc") -> Right Desc
    _ -> Left "sort order invalido"

  return (f, ord)

jsonToSortField _ = Left "sort field mal formado"


-------------------------------------------------------
-- Termianl -> JSON
-------------------------------------------------------
terminalToJson :: QueryTerminal -> A.Value
terminalToJson TerminalPreview =
  obj "preview" []

terminalToJson (TerminalSave path) =
  obj "save" [("path", A.String (text path))]

-------------------------------------------------------
-- JSON -> Terminal
-------------------------------------------------------
jsonToTerminal :: A.Value -> Either String QueryTerminal
jsonToTerminal (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of

    Just (A.String "preview") ->
      Right TerminalPreview

    Just (A.String "save") ->
      case KM.lookup (stringToKey "path") obj of
        Just (A.String t) -> Right (TerminalSave (str t))
        _ -> Left "save sin path"

    _ -> Left "terminal desconocido"

jsonToTerminal _ = Left "terminal invalido"


-------------------------------------------------------
-- GroupSpec -> JSON
-------------------------------------------------------
groupSpecToJson :: GroupSpec -> A.Value
groupSpecToJson (GroupSpec fields aggs having) =
  A.Object (KM.fromList
    [ (stringToKey "fields", A.Array (V.fromList (map (A.String . text) fields)))
    , (stringToKey "aggs", A.Array (V.fromList (map aggregateToJson aggs)))
    , (stringToKey "having",
        maybe A.Null boolExpToJson having)
    ])

-------------------------------------------------------
-- JSON -> GroupSpec
-------------------------------------------------------
jsonToGroupSpec :: A.Value -> Either String GroupSpec
jsonToGroupSpec (A.Object obj) = do
  fields <- case KM.lookup (stringToKey "fields") obj of
    Just (A.Array arr) ->
      Right [ str t | A.String t <- V.toList arr ]
    _ -> Left "group sin fields"
  aggs <- case KM.lookup (stringToKey "aggs") obj of
    Just (A.Array arr) ->
      mapM jsonToAggregate (V.toList arr)
    _ -> Right []
  having <- case KM.lookup (stringToKey "having") obj of
    Just A.Null -> Right Nothing
    Just v -> fmap Just (jsonToBoolExp v)
    Nothing -> Right Nothing
  return (GroupSpec fields aggs having)
jsonToGroupSpec _ = Left "group invalido"

-------------------------------------------------------
-- Aggregate -> JSON
-------------------------------------------------------
aggregateToJson :: Aggregate -> A.Value
aggregateToJson (Aggregate f field alias) =
  A.Object (KM.fromList
    [ (stringToKey "func", A.String (text (show f)))
    , (stringToKey "field", A.String (text field))
    , (stringToKey "alias", A.String (text alias))
    ])

-------------------------------------------------------
-- JSON -> Aggregate
-------------------------------------------------------
jsonToAggregate :: A.Value -> Either String Aggregate
jsonToAggregate (A.Object obj) = do
  func <- case KM.lookup (stringToKey "func") obj of
    Just (A.String "AggCount") -> Right AggCount
    Just (A.String "AggSum") -> Right AggSum
    Just (A.String "AggAvg") -> Right AggAvg
    Just (A.String "AggMin") -> Right AggMin
    Just (A.String "AggMax") -> Right AggMax
    _ -> Left "agg func invalida"
  field <- case KM.lookup (stringToKey "field") obj of
    Just (A.String t) -> Right (str t)
    _ -> Left "agg field invalido"
  alias <- case KM.lookup (stringToKey "alias") obj of
    Just (A.String t) -> Right (str t)
    _ -> Left "agg alias invalido"
  return (Aggregate func field alias)

jsonToAggregate _ = Left "aggregate invalido"

-------------------------------------------------------
-- HELPERS DE KEYS / TEXT
-------------------------------------------------------
keyToString :: K.Key -> String
keyToString = T.unpack . K.toText

stringToKey :: String -> K.Key
stringToKey = K.fromText . T.pack

-- este metodo no lo usamos mas (por ahora no se borra)
textToKey :: T.Text -> K.Key
textToKey = K.fromText

-- este tampoco lo usamos mas (por ahora no se borra)
keyToText :: K.Key -> T.Text
keyToText = K.toText

text :: String -> T.Text
text = T.pack

str :: T.Text -> String
str = T.unpack


-------------------------------------------------------
-- HELPERS GENERICOS P JSON
-------------------------------------------------------
getField :: String -> KM.KeyMap A.Value -> Either String A.Value
getField k obj =
  maybe (Left ("Falta campo " ++ k)) Right
    (KM.lookup (stringToKey k) obj)

getStringField :: String -> KM.KeyMap A.Value -> Either String String
getStringField k obj = do
  v <- getField k obj
  case v of
    A.String t -> Right (T.unpack t)
    _ -> Left (k ++ " no es string")

getIntField :: String -> KM.KeyMap A.Value -> Either String Int
getIntField k obj = do
  v <- getField k obj
  case v of
    A.Number n ->
      case floatingOrInteger n of
        Right i -> Right i
        _ -> Left (k ++ " no es entero")
    _ -> Left (k ++ " no es numero")

getArrayField :: String -> KM.KeyMap A.Value -> Either String [A.Value]
getArrayField k obj = do
  v <- getField k obj
  case v of
    A.Array arr -> Right (V.toList arr)
    _ -> Left (k ++ " no es array")

obj :: T.Text -> [(T.Text, A.Value)] -> A.Value
obj t fields =
  A.Object (KM.fromList
    ( (stringToKey "type", A.String t)
    : [ (stringToKey (str k), v) | (k,v) <- fields ]
    ))
