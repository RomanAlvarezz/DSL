{-# LANGUAGE OverloadedStrings #-}

--module JSONAdapter( jsonToDatabase, databaseToJson, jsonToValue, valueToJson) where
module JSONAdapter(
  -- ya existentes
  jsonToDatabase,
  databaseToJson,
  jsonToValue,
  valueToJson,

  -- para snapshots
  databaseToJsonSnap,
  jsonToDatabaseSnap,
  timestampSnapshotToJson,
  jsonToTimestampSnapshot,

  -- para views
  findToJson,
  jsonToFind,

  -- helper necesario
  parseCollection
) where
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
--import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Scientific (floatingOrInteger, Scientific, scientific)

import AST
import Prelude hiding (Eq)
import Value hiding (FieldName)
--import Evaluator

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

jsonToValue (A.Number n) =
  case floatingOrInteger n of
    Left f  -> VFloat f
    Right i -> VInt i

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
-- BoolExp -> JSON
-------------------------------------------------------

boolExpToJson :: BoolExp -> A.Value

boolExpToJson BTrue = obj "true" []
boolExpToJson BFalse = obj "false" []

boolExpToJson (Not b) =
  obj "not" [("value", boolExpToJson b)]

boolExpToJson (And a b) =
  obj "and" [("l", boolExpToJson a), ("r", boolExpToJson b)]

boolExpToJson (Or a b) =
  obj "or" [("l", boolExpToJson a), ("r", boolExpToJson b)]

boolExpToJson (Eq a b) =
  obj "eq" [("l", expToJson a), ("r", expToJson b)]

boolExpToJson (Neq a b) =
  obj "neq" [("l", expToJson a), ("r", expToJson b)]

boolExpToJson (Gt a b) =
  obj "gt" [("l", expToJson a), ("r", expToJson b)]

boolExpToJson (Ge a b) =
  obj "ge" [("l", expToJson a), ("r", expToJson b)]

boolExpToJson (Lt a b) =
  obj "lt" [("l", expToJson a), ("r", expToJson b)]

boolExpToJson (Le a b) =
  obj "le" [("l", expToJson a), ("r", expToJson b)]

boolExpToJson (Exists e) =
  obj "exists" [("value", expToJson e)]

-------------------------------------------------------
-- JSON -> BoolExp
-------------------------------------------------------

jsonToBoolExp :: A.Value -> Either String BoolExp
jsonToBoolExp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of

    Just (A.String "true") -> Right BTrue
    Just (A.String "false") -> Right BFalse

    Just (A.String "not") -> do
      v <- getField "value" obj
      Not <$> jsonToBoolExp v

    Just (A.String "and") -> do
      l <- getField "l" obj
      r <- getField "r" obj
      And <$> jsonToBoolExp l <*> jsonToBoolExp r

    Just (A.String "or") -> do
      l <- getField "l" obj
      r <- getField "r" obj
      Or <$> jsonToBoolExp l <*> jsonToBoolExp r

    Just (A.String "eq") ->
      bin Eq

    Just (A.String "neq") ->
      bin Neq

    Just (A.String "gt") ->
      bin Gt

    Just (A.String "ge") ->
      bin Ge

    Just (A.String "lt") ->
      bin Lt

    Just (A.String "le") ->
      bin Le

    Just (A.String "exists") -> do
      v <- getField "value" obj
      Exists <$> jsonToExp v

    _ -> Left "BoolExp desconocido"

  where
    bin cons = do
      l <- getField "l" obj
      r <- getField "r" obj
      cons <$> jsonToExp l <*> jsonToExp r

jsonToBoolExp _ = Left "BoolExp invalido"

-------------------------------------------------------
-- Exp -> JSON
-------------------------------------------------------

expToJson :: Exp -> A.Value

expToJson (IntExp n) = obj "int" [("value", A.Number (fromIntegral n))]
expToJson (FloatExp f) = obj "float" [("value", A.Number (realToFrac f))]
expToJson (StringExp s) = obj "string" [("value", A.String (text s))]
expToJson (BoolExpVal b) = obj "bool" [("value", A.Bool b)]
expToJson NullExp = obj "null" []

expToJson (VarExp f) = obj "var" [("name", A.String (text f))]

expToJson (AddExp a b) =
  obj "add" [("l", expToJson a), ("r", expToJson b)]

expToJson (SubExp a b) =
  obj "sub" [("l", expToJson a), ("r", expToJson b)]

expToJson (MulExp a b) =
  obj "mul" [("l", expToJson a), ("r", expToJson b)]

expToJson (DivExp a b) =
  obj "div" [("l", expToJson a), ("r", expToJson b)]

expToJson (FieldAccess e f) =
  obj "fieldAccess"
    [ ("exp", expToJson e)
    , ("field", A.String (text f))
    ]

expToJson (JObjectExp fields) =
  obj "object"
    [ ("fields", A.Array (V.fromList (map fieldToJson fields))) ]

expToJson (JArrayExp xs) =
  obj "array"
    [ ("values", A.Array (V.fromList (map expToJson xs))) ]


-- Helpers

obj :: T.Text -> [(T.Text, A.Value)] -> A.Value
obj t fields =
  A.Object (KM.fromList
    ( (stringToKey "type", A.String t)
    : [ (stringToKey (str k), v) | (k,v) <- fields ]
    ))

fieldToJson :: (FieldName, Exp) -> A.Value
fieldToJson (k,v) =
  A.Object (KM.fromList
    [ (stringToKey "k", A.String (text k))
    , (stringToKey "v", expToJson v)
    ])

-------------------------------------------------------
-- JSON -> Exp
-------------------------------------------------------

jsonToExp :: A.Value -> Either String Exp
jsonToExp (A.Object obj) =
  case KM.lookup (stringToKey "type") obj of

    Just (A.String "int") -> do
      v <- getNum
      Right (IntExp v)

    Just (A.String "float") -> do
      v <- getFloat
      Right (FloatExp v)

    Just (A.String "string") -> do
      v <- getStr
      Right (StringExp v)

    Just (A.String "bool") -> do
      v <- getBool
      Right (BoolExpVal v)

    Just (A.String "null") ->
      Right NullExp

    Just (A.String "var") -> do
      name <- getStringField "name" obj
      Right (VarExp name)

    Just (A.String "add") -> do
      bin AddExp

    Just (A.String "sub") ->  do
      bin SubExp

    Just (A.String "mul") -> do
      bin MulExp

    Just (A.String "div") -> do
      bin DivExp

    Just (A.String "fieldAccess") -> do
      e <- getField "exp" obj
      f <- getStringField "field" obj
      FieldAccess <$> jsonToExp e <*> pure f

    Just (A.String "object") -> do
      arr <- getArrayField "fields" obj
      fields <- mapM jsonToField arr
      Right (JObjectExp fields)

    Just (A.String "array") -> do
      arr <- getArrayField "values" obj
      xs <- mapM jsonToExp arr
      Right (JArrayExp xs)
    _ -> Left "Exp desconocida"

  where
    getStr = do
      v <- getField "value" obj
      case v of
        A.String t -> Right (str t)
        _ -> Left "No es string"

    getBool = do
      v <- getField "value" obj
      case v of
        A.Bool b -> Right b
        _ -> Left "No es bool"

    getNum = do
      v <- getField "value" obj
      case v of
        A.Number n ->
          case floatingOrInteger n of
            Right i -> Right i
            _ -> Left "No es int"
        _ -> Left "No es numero"

    getFloat = do
      v <- getField "value" obj
      case v of
        A.Number n ->
          case floatingOrInteger n of
            Left f -> Right f
            Right i -> Right (fromIntegral i)
        _ -> Left "No es numero"


    bin cons = do
      l <- getField "l" obj
      r <- getField "r" obj
      cons <$> jsonToExp l <*> jsonToExp r


jsonToField :: A.Value -> Either String (FieldName, Exp)
jsonToField (A.Object obj) = do
  k <- case KM.lookup (stringToKey "k") obj of
    Just (A.String t) -> Right (str t)
    _ -> Left "field key invalida"

  v <- case KM.lookup (stringToKey "v") obj of
    Just val -> jsonToExp val
    Nothing -> Left "field sin valor"

  return (k,v)

jsonToField _ = Left "field mal formado"


-------------------------------------------------------
-- HELPERS DE KEYS / TEXT
-------------------------------------------------------

keyToString :: K.Key -> String
keyToString = T.unpack . K.toText

stringToKey :: String -> K.Key
stringToKey = K.fromText . T.pack

textToKey :: T.Text -> K.Key
textToKey = K.fromText

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
