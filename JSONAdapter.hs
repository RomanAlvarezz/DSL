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
        [ (K.fromText (T.pack k), valueToJson v)
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
        [ (T.unpack (K.toText collName), parseCollection val)
        | (collName, val) <- collections
        ]

  in Right (db, nextId)

jsonToDatabase _ =
  Left "El JSON de la base debe ser un objeto"

parseCollection :: A.Value -> [Document]

parseCollection (A.Array arr) =
  map parseDocument (V.toList arr)

parseCollection _ = []

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
      | (coll, docs) <- db
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


-------------------------------------------------------
-- database -> Json (sin nextId, para timestamps)
-------------------------------------------------------

databaseToJsonSnap :: Database -> A.Value
databaseToJsonSnap db =
  A.Object (
    KM.fromList
      [ (K.fromText (T.pack coll), collectionToJson docs)
      | (coll, docs) <- db
      ]
  )

-------------------------------------------------------
-- Json -> database (sin nextId, para timestamps)
-------------------------------------------------------

jsonToDatabaseSnap :: A.Value -> Either String Database
jsonToDatabaseSnap (A.Object obj) =
  Right
      [ (T.unpack (K.toText collName), parseCollection val)
      | (collName, val) <- KM.toList obj
      ]

jsonToDatabaseSnap _ =
  Left "El snapshot de la base debe ser un objeto"


-------------------------------------------------------
-- timestamp -> Json
-------------------------------------------------------

timestampSnapshotToJson :: TimestampSnapshot -> A.Value
timestampSnapshotToJson (DBSnapshot db) =
  A.Object (KM.fromList
    [ (K.fromText "type", A.String "db")
    , (K.fromText "data", databaseToJsonSnap db)
    ])

timestampSnapshotToJson (CollSnapshot coll docs) =
  A.Object (KM.fromList
    [ (K.fromText "type", A.String "collection")
    , (K.fromText "collection", A.String (T.pack coll))
    , (K.fromText "data", collectionToJson docs)
    ])


-------------------------------------------------------
-- Json -> timestamp
-------------------------------------------------------

jsonToTimestampSnapshot :: A.Value -> Either String TimestampSnapshot
jsonToTimestampSnapshot (A.Object obj) =
  case KM.lookup (K.fromText "type") obj of

    Just (A.String "db") ->
      case KM.lookup (K.fromText "data") obj of
        Just dbVal -> do
          db <- jsonToDatabaseSnap dbVal
          return (DBSnapshot db)
        Nothing ->
          Left "Falta campo 'data' en snapshot db"

    Just (A.String "collection") ->
      case ( KM.lookup (K.fromText "collection") obj
           , KM.lookup (K.fromText "data") obj ) of

        (Just (A.String coll), Just docsVal) ->
          let docs = parseCollection docsVal
          in Right (CollSnapshot (T.unpack coll) docs)

        _ ->
          Left "Snapshot collection mal formado"

    _ ->
      Left "Tipo de snapshot desconocido"

jsonToTimestampSnapshot _ =
  Left "Snapshot debe ser un objeto JSON"



-- TODAVIA NO SOPORTA AddExp, SubExp, FieldAccess, JObjectExp...
-------------------------------------------------------
-- FIND -> JSON
-------------------------------------------------------

findToJson :: Find -> A.Value
findToJson (Find coll ops term) =
  A.Object (KM.fromList
    [ (K.fromText "collection", A.String (T.pack coll))
    , (K.fromText "pipeline", A.Array (V.fromList (map queryOpToJson ops)))
    , (K.fromText "terminal", terminalToJson term)
    ])


-------------------------------------------------------
-- JSON -> Find
-------------------------------------------------------


jsonToFind :: A.Value -> Either String Find
jsonToFind (A.Object obj) = do

  coll <- case KM.lookup (K.fromText "collection") obj of
    Just (A.String t) -> Right (T.unpack t)
    _ -> Left "Find sin collection"

  ops <- case KM.lookup (K.fromText "pipeline") obj of
    Just (A.Array arr) ->
      mapM jsonToQueryOp (V.toList arr)
    _ -> Right []

  term <- case KM.lookup (K.fromText "terminal") obj of
    Just v -> jsonToTerminal v
    Nothing -> Left "Find sin terminal"

  return (Find coll ops term)

jsonToFind _ = Left "Find debe ser objeto"



-------------------------------------------------------
-- QueryOp -> JSON
-------------------------------------------------------

queryOpToJson :: QueryOp -> A.Value

queryOpToJson (QFilter cond) =
  obj "filter" [("cond", boolExpToJson cond)]

queryOpToJson (QSelect fields) =
  obj "select" [("fields", A.Array (V.fromList (map (A.String . T.pack) fields)))]

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
  case KM.lookup (K.fromText "type") obj of

    Just (A.String "filter") -> do
      condVal <- getField "cond"
      cond <- jsonToBoolExp condVal
      return (QFilter cond)

    Just (A.String "select") -> do
      arr <- getArray "fields"
      let fields = [ T.unpack t | A.String t <- arr ]
      return (QSelect fields)

    Just (A.String "limit") -> do
      n <- getInt "value"
      return (QLimit n)

    Just (A.String "sort") -> do
      arr <- getArray "fields"
      fs <- mapM jsonToSortField arr
      return (QSort fs)

    Just (A.String "group") -> do
      specVal <- getField "spec"
      spec <- jsonToGroupSpec specVal
      return (QGroup spec)

    _ -> Left "QueryOp desconocido"

  where
    getField k =
      maybe (Left ("Falta campo " ++ k)) Right
        (KM.lookup (K.fromText (T.pack k)) obj)

    getArray k = do
      v <- getField k
      case v of
        A.Array arr -> Right (V.toList arr)
        _ -> Left ("Campo " ++ k ++ " no es array")

    getInt k = do
      v <- getField k
      case v of
        A.Number n ->
          case floatingOrInteger n of
            Right i -> Right i
            _ -> Left "No es entero"
        _ -> Left "No es numero"

jsonToQueryOp _ = Left "QueryOp debe ser objeto"


-------------------------------------------------------
-- Sort -> JSON
-------------------------------------------------------

sortFieldToJson :: (FieldName, SortOrder) -> A.Value
sortFieldToJson (f, ord) =
  A.Object (KM.fromList
    [ (K.fromText "field", A.String (T.pack f))
    , (K.fromText "order", A.String (T.pack (show ord)))
    ])

-------------------------------------------------------
-- JSON -> Sort
-------------------------------------------------------

jsonToSortField :: A.Value -> Either String (FieldName, SortOrder)
jsonToSortField (A.Object obj) = do
  f <- case KM.lookup "field" obj of
    Just (A.String t) -> Right (T.unpack t)
    _ -> Left "sort field invalido"

  ord <- case KM.lookup "order" obj of
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
  obj "save" [("path", A.String (T.pack path))]

-------------------------------------------------------
-- JSON -> Terminal
-------------------------------------------------------

jsonToTerminal :: A.Value -> Either String QueryTerminal
jsonToTerminal (A.Object obj) =
  case KM.lookup "type" obj of

    Just (A.String "preview") ->
      Right TerminalPreview

    Just (A.String "save") ->
      case KM.lookup "path" obj of
        Just (A.String t) -> Right (TerminalSave (T.unpack t))
        _ -> Left "save sin path"

    _ -> Left "terminal desconocido"

jsonToTerminal _ = Left "terminal invalido"


-------------------------------------------------------
-- GroupSpec -> JSON
-------------------------------------------------------

groupSpecToJson :: GroupSpec -> A.Value
groupSpecToJson (GroupSpec fields aggs having) =
  A.Object (KM.fromList
    [ (K.fromText "fields", A.Array (V.fromList (map (A.String . T.pack) fields)))
    , (K.fromText "aggs", A.Array (V.fromList (map aggregateToJson aggs)))
    , (K.fromText "having",
        maybe A.Null boolExpToJson having)
    ])

-------------------------------------------------------
-- JSON -> GroupSpec
-------------------------------------------------------

jsonToGroupSpec :: A.Value -> Either String GroupSpec
jsonToGroupSpec (A.Object obj) = do

  fields <- case KM.lookup "fields" obj of
    Just (A.Array arr) ->
      Right [ T.unpack t | A.String t <- V.toList arr ]
    _ -> Left "group sin fields"

  aggs <- case KM.lookup "aggs" obj of
    Just (A.Array arr) ->
      mapM jsonToAggregate (V.toList arr)
    _ -> Right []

  having <- case KM.lookup "having" obj of
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
    [ (K.fromText "func", A.String (T.pack (show f)))
    , (K.fromText "field", A.String (T.pack field))
    , (K.fromText "alias", A.String (T.pack alias))
    ])

-------------------------------------------------------
-- JSON -> Aggregate
-------------------------------------------------------

jsonToAggregate :: A.Value -> Either String Aggregate
jsonToAggregate (A.Object obj) = do

  func <- case KM.lookup "func" obj of
    Just (A.String "AggCount") -> Right AggCount
    Just (A.String "AggSum") -> Right AggSum
    Just (A.String "AggAvg") -> Right AggAvg
    Just (A.String "AggMin") -> Right AggMin
    Just (A.String "AggMax") -> Right AggMax
    _ -> Left "agg func invalida"

  field <- case KM.lookup "field" obj of
    Just (A.String t) -> Right (T.unpack t)
    _ -> Left "agg field invalido"

  alias <- case KM.lookup "alias" obj of
    Just (A.String t) -> Right (T.unpack t)
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
  case KM.lookup "type" obj of

    Just (A.String "true") -> Right BTrue
    Just (A.String "false") -> Right BFalse

    Just (A.String "not") -> do
      v <- get "value"
      Not <$> jsonToBoolExp v

    Just (A.String "and") -> do
      l <- get "l"
      r <- get "r"
      And <$> jsonToBoolExp l <*> jsonToBoolExp r

    Just (A.String "or") -> do
      l <- get "l"
      r <- get "r"
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
      v <- get "value"
      Exists <$> jsonToExp v

    _ -> Left "BoolExp desconocido"

  where
    get k =
      maybe (Left ("Falta campo " ++ k)) Right
        (KM.lookup (K.fromText (T.pack k)) obj)

    bin cons = do
      l <- get "l"
      r <- get "r"
      cons <$> jsonToExp l <*> jsonToExp r

jsonToBoolExp _ = Left "BoolExp invalido"

-------------------------------------------------------
-- Exp -> JSON
-------------------------------------------------------

expToJson :: Exp -> A.Value

expToJson (IntExp n) = obj "int" [("value", A.Number (fromIntegral n))]
expToJson (FloatExp f) = obj "float" [("value", A.Number (realToFrac f))]
expToJson (StringExp s) = obj "string" [("value", A.String (T.pack s))]
expToJson (BoolExpVal b) = obj "bool" [("value", A.Bool b)]
expToJson NullExp = obj "null" []

expToJson (VarExp f) = obj "var" [("name", A.String (T.pack f))]

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
    , ("field", A.String (T.pack f))
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
    ( (K.fromText "type", A.String t)
    : [ (K.fromText k, v) | (k,v) <- fields ]
    ))

fieldToJson :: (FieldName, Exp) -> A.Value
fieldToJson (k,v) =
  A.Object (KM.fromList
    [ (K.fromText "k", A.String (T.pack k))
    , (K.fromText "v", expToJson v)
    ])

-------------------------------------------------------
-- JSON -> Exp
-------------------------------------------------------

jsonToExp :: A.Value -> Either String Exp
jsonToExp (A.Object obj) =
  case KM.lookup "type" obj of

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
      name <- getStrField "name"
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
      e <- getField "exp"
      f <- getStrField "field"
      FieldAccess <$> jsonToExp e <*> pure f

    Just (A.String "object") -> do
      arr <- getArray "fields"
      fields <- mapM jsonToField arr
      Right (JObjectExp fields)

    Just (A.String "array") -> do
      arr <- getArray "values"
      xs <- mapM jsonToExp arr
      Right (JArrayExp xs)
    _ -> Left "Exp desconocida"

  where
    getField k =
      maybe (Left ("Falta campo " ++ k)) Right
        (KM.lookup (K.fromText (T.pack k)) obj)

    getStrField k = do
      v <- getField k
      case v of
        A.String t -> Right (T.unpack t)
        _ -> Left (k ++ " no es string")

    getStr = do
      v <- getField "value"
      case v of
        A.String t -> Right (T.unpack t)
        _ -> Left "No es string"

    getBool = do
      v <- getField "value"
      case v of
        A.Bool b -> Right b
        _ -> Left "No es bool"

    getNum = do
      v <- getField "value"
      case v of
        A.Number n ->
          case floatingOrInteger n of
            Right i -> Right i
            _ -> Left "No es int"
        _ -> Left "No es numero"

    getFloat = do
      v <- getField "value"
      case v of
        A.Number n ->
          case floatingOrInteger n of
            Left f -> Right f
            Right i -> Right (fromIntegral i)
        _ -> Left "No es numero"

    getArray k = do
      v <- getField k
      case v of
        A.Array arr -> Right (V.toList arr)
        _ -> Left (k ++ " no es array")

    bin cons = do
      l <- getField "l"
      r <- getField "r"
      cons <$> jsonToExp l <*> jsonToExp r


jsonToField :: A.Value -> Either String (FieldName, Exp)
jsonToField (A.Object obj) = do
  k <- case KM.lookup "k" obj of
    Just (A.String t) -> Right (T.unpack t)
    _ -> Left "field key invalida"

  v <- case KM.lookup "v" obj of
    Just val -> jsonToExp val
    Nothing -> Left "field sin valor"

  return (k,v)

jsonToField _ = Left "field mal formado"
