module Evaluator where

import AST
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Data.List (find, groupBy, sortOn, sortBy)
import System.IO

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V
import qualified Data.Text as T


-------------------------------------------------------
-- VALORES JSON INTERNOS
-------------------------------------------------------

data Value
  = VInt Int
  | VFloat Double
  | VString String
  | VBool Bool
  | VNull
  | VObject [(FieldName, Value)]
  | VArray [Value]
  deriving (Show, Eq, Ord)



type Document = [(FieldName, Value)]
type CollectionData = [Document]
type Database = M.Map Collection CollectionData

-------------------------------------------------------
-- SNAPSHOTS PARA TIMESTAMPS
-------------------------------------------------------

data TimestampSnapshot
  = DBSnapshot Database
  | CollSnapshot Collection CollectionData
  deriving (Show)

-------------------------------------------------------
-- CONTEXTO DE RUNTIME
-------------------------------------------------------

data RuntimeContext = RuntimeContext
  { views :: M.Map ViewName Find
  , timestamps :: M.Map TimestampLabel TimestampSnapshot
  }

-------------------------------------------------------
-- ESTADO GLOBAL DEL EVALUADOR
-------------------------------------------------------

data EvalState = EvalState
  { database :: Database
  , runtime  :: RuntimeContext
  , nextId   :: Int
  }

-------------------------------------------------------
-- ERRORES DEL EVALUADOR
-------------------------------------------------------

data EvalError
  = CollectionNotFound Collection
  | ViewNotFound ViewName
  | TimestampNotFound TimestampLabel
  | InvalidTimestampTarget
  | TypeError
  | DivisionByZero
  | ReservedField
  | FieldNotFoundInObject String
  deriving (Show, Eq)

showError :: EvalError -> String
showError (CollectionNotFound c) =
  "Colección '" ++ c ++ "' no encontrada"

showError (ViewNotFound v) =
  "Vista '" ++ v ++ "' no encontrada"

showError (TimestampNotFound t) =
  "Timestamp '" ++ t ++ "' no encontrado"

showError InvalidTimestampTarget =
  "El tipo de rollback no coincide con el timestamp"

showError TypeError =
  "Error de tipo en la operación"

showError DivisionByZero =
  "División por cero"

showError ReservedField =
  "No se puede usar el campo reservado '_id'"

showError (FieldNotFoundInObject f) =
  "Campo '" ++ f ++ "' no encontrado"

-------------------------------------------------------
-- MONADA DEL EVALUADOR
-------------------------------------------------------

type Eval a =
  StateT EvalState (ExceptT EvalError IO) a

-------------------------------------------------------
-- PROGRAMA
-------------------------------------------------------

evalProgram :: Program -> Eval ()
evalProgram = evalComm

-------------------------------------------------------
-- COMANDOS
-------------------------------------------------------

evalComm :: Comm -> Eval ()

evalComm Skip = return ()

evalComm (Seq c1 c2) = do
  evalComm c1
  evalComm c2

-------------------------------------------------------
-- CREAR / BORRAR COLECCIONES
-------------------------------------------------------

evalComm (CommCreateColl name) =
  updateDatabase (M.insert name [])

evalComm (CommDropColl name) = do
  st <- get
  let db = database st
  if M.member name db
    then updateDatabase (M.delete name)
    else throwError (CollectionNotFound name)

-------------------------------------------------------
-- INSERT
-------------------------------------------------------
evalComm (CommInsert coll exp) = do
  doc <- evalExpAsDoc exp

  ---------------------------------------------------
  -- Validación reutilizable: el documento no debe
  -- contener "_id" definido por el usuario
  ---------------------------------------------------
  case validateNoIdField doc of
    Nothing -> throwError ReservedField
    Just cleanDoc -> do
      st <- get
      let db = database st
      let newId = nextId st
      let docWithId = ("_id", VInt newId) : cleanDoc
      case M.lookup coll db of
        Nothing -> throwError (CollectionNotFound coll)
        Just docs -> do 
          updateDatabase (M.insert coll (docWithId : docs))
          updateDatabaseNextId


--evalComm (CommInsertMany coll exps) =
--  mapM_ (evalComm . CommInsert coll) exps
evalComm (CommInsertMany coll []) = return ()
evalComm (CommInsertMany coll (e:es)) = do
  evalComm (CommInsert coll e)
  evalComm (CommInsertMany coll es)

-------------------------------------------------------
-- DELETE
-------------------------------------------------------

evalComm (CommDelete coll cond) = do
  st <- get
  case M.lookup coll (database st) of
    Nothing -> throwError (CollectionNotFound coll)
    Just docs -> do
      docs' <- filterM (\d -> fmap not (evalBool cond d)) docs
      updateDatabase (M.insert coll docs')



-------------------------------------------------------
-- UPDATE ONE
-------------------------------------------------------
evalComm (CommUpdateOne coll cond exp) = do
  newDoc <- evalExpAsDoc exp

  case validateNoIdField newDoc of
    Nothing -> throwError ReservedField
    Just cleanDoc -> do

      st <- get

      case M.lookup coll (database st) of
        Nothing -> throwError (CollectionNotFound coll)

        Just docs -> do
          docs' <- updateDocs True cond cleanDoc docs
          updateDatabase (M.insert coll docs')

evalComm (CommUpdateMany coll cond exp) = do
 newDoc <- evalExpAsDoc exp
 case validateNoIdField newDoc of
   Nothing -> throwError ReservedField
   Just cleanDoc -> do
     st <- get

     case M.lookup coll (database st) of
       Nothing -> throwError (CollectionNotFound coll)

       Just docs -> do
         docs' <- updateDocs False cond cleanDoc docs
         updateDatabase (M.insert coll docs')


-------------------------------------------------------
-- CONSULTAS
-------------------------------------------------------

evalComm (CommQuery find) = do
  res <- evalFind find
  evalTerminal (getTerminal find) res

-------------------------------------------------------
-- VISTAS
-------------------------------------------------------

evalComm (CommCreateView name find) =
  updateViews (M.insert name find)


evalComm (CommUseView name ViewOnly) = do
  st <- get
  case M.lookup name (views (runtime st)) of
    Nothing -> throwError (ViewNotFound name)
    Just f  -> evalComm (CommQuery f)

evalComm (CommUseView name (ViewWithPipeline f)) = do
  st <- get
  case M.lookup name (views (runtime st)) of
    Nothing -> throwError (ViewNotFound name)
    Just (Find coll ops _) ->
      evalComm (CommQuery (Find coll (ops ++ getOps f) (getTerminal f)))

-------------------------------------------------------
-- TIMESTAMPS
-------------------------------------------------------

evalComm (CommTimestamp target label) = do
  st <- get
  let db = database st
  let rt = runtime st

  snap <- case target of
    TSDatabase ->
      return (DBSnapshot db)

    TSColl coll ->
      case M.lookup coll db of
        Nothing -> throwError (CollectionNotFound coll)
        Just docs -> return (CollSnapshot coll docs)

  updateTimestamps (M.insert label snap)

-------------------------------------------------------
-- ROLLBACK
-------------------------------------------------------

evalComm (CommRollback target label) = do
  st <- get
  let rt = runtime st

  snap <- case M.lookup label (timestamps rt) of
    Nothing -> throwError (TimestampNotFound label)
    Just s  -> return s

  case (target, snap) of

    (TSDatabase, DBSnapshot db') ->
      put st { database = db' }

    (TSColl coll, CollSnapshot c docs)
      | coll == c ->
          let db' = M.insert coll docs (database st)
          in put st { database = db' }

    _ -> throwError InvalidTimestampTarget

-------------------------------------------------------
-- TRANSACCIONES
-------------------------------------------------------

evalComm (CommTransaction comms) = do
  snapshot <- get
  catchError
    (mapM_ evalComm comms)
    (\_ -> put snapshot)

-------------------------------------------------------
-- EVALUACION DE CONSULTAS
-------------------------------------------------------

evalFind :: Find -> Eval [Document]
evalFind (Find coll ops term) = do
  st <- get
  case M.lookup coll (database st) of
    Nothing -> throwError (CollectionNotFound coll)
    Just docs -> applyPipeline docs ops


applyPipeline :: [Document] -> [QueryOp] -> Eval [Document]
applyPipeline docs [] = return docs
applyPipeline docs (op:rest) = do
  docs' <- applyOp docs op
  applyPipeline docs' rest

-------------------------------------------------------
-- GROUP
-------------------------------------------------------

lookupField :: FieldName -> Document -> Value
lookupField f doc =
  case lookup f doc of
    Just v  -> v
    Nothing -> VNull

groupKey :: [FieldName] -> Document -> [Value]
groupKey fields doc =
  map (\f -> lookupField f doc) fields

groupDocuments :: [FieldName] -> [Document] -> [[Document]]
groupDocuments fields docs =
  groupBy sameKey sorted
  where
    sorted = sortOn (groupKey fields) docs
    sameKey d1 d2 = groupKey fields d1 == groupKey fields d2

buildGroupDoc :: [FieldName] -> [Aggregate] -> [Document] -> Document
buildGroupDoc fields aggs docs =
  let keyVals = map (\f -> (f, lookupField f (head docs))) fields
      aggVals = map (`applyAggregate` docs) aggs
  in keyVals ++ aggVals

applyAggregate :: Aggregate -> [Document] -> (FieldName, Value)

applyAggregate (Aggregate AggCount _ alias) docs =
  (alias, VInt (length docs))

applyAggregate (Aggregate AggSum field alias) docs =
  let vals = [n | doc <- docs, Just (VInt n) <- [lookup field doc]]
  in (alias, VInt (sum vals))
{--
applyAggregate (Aggregate AggAvg field alias) docs =
  let vals = [n | doc <- docs, Just (VInt n) <- [lookup field doc]]
      s = sum vals
      c = length vals
  in (alias, VFloat (fromIntegral s / fromIntegral c))
--}
applyAggregate (Aggregate AggAvg field alias) docs =
  let vals = [n | doc <- docs, Just (VInt n) <- [lookup field doc]]
      s = sum vals
      c = length vals
      truncate3 x =
        let factor = 1000
        in fromIntegral (floor (x * factor)) / factor
  in if c == 0
        then (alias, VNull)
        else
          let avg = fromIntegral s / fromIntegral c
          in (alias, VFloat (truncate3 avg))

applyAggregate (Aggregate AggMin field alias) docs =
  let vals = [v | doc <- docs, Just v <- [lookup field doc]]
  in if null vals
        then (alias, VNull)
        else (alias, minimum vals)

applyAggregate (Aggregate AggMax field alias) docs =
  let vals = [v | doc <- docs, Just v <- [lookup field doc]]
  in if null vals
        then (alias, VNull)
        else (alias, maximum vals)

{--
applyAggregate (Aggregate AggMin field alias) docs =
  let vals = [v | doc <- docs, Just v <- [lookup field doc]]
  in (alias, minimum vals)

applyAggregate (Aggregate AggMax field alias) docs =
  let vals = [v | doc <- docs, Just v <- [lookup field doc]]
  in (alias, maximum vals)
--}
-------------------------------------------------------
-- PIPELINE
-------------------------------------------------------

applyOp :: [Document] -> QueryOp -> Eval [Document]

--applyOp docs (QFilter cond) =
--  filterM (evalBool cond) docs
applyOp docs (QFilter cond) =
  filterM safeEval docs
  where
    safeEval doc =
      catchError
        (evalBool cond doc)
        (\_ -> return False)

applyOp docs (QSelect fields) =
  return (map (selectFields fields) docs)

applyOp docs (QLimit n) =
  return (take n docs)

applyOp docs (QSort fields) =
  return (sortBy cmp docs)
  where
    cmp d1 d2 = compareFields fields d1 d2

    compareFields [] _ _ = EQ

    compareFields ((field, order):rest) d1 d2 =
      case (lookup field d1, lookup field d2) of
        (Just v1, Just v2) ->
          let res = case order of
                      Asc  -> compare v1 v2
                      Desc -> compare v2 v1
          in if res == EQ
                then compareFields rest d1 d2
                else res
        _ -> compareFields rest d1 d2


applyOp docs (QGroup (GroupSpec fields aggs having)) = do

  let groups = groupDocuments fields docs

  let groupedDocs =
        map (buildGroupDoc fields aggs) groups

  case having of
    Nothing ->
      return groupedDocs

--    Just cond ->
--      filterM (evalBool cond) groupedDocs

    Just cond ->
      filterM safeEval groupedDocs
      where
        safeEval doc =
          catchError
            (evalBool cond doc)
            (\_ -> return False)

-------------------------------------------------------
-- TERMINALES
-------------------------------------------------------
evalTerminal :: QueryTerminal -> [Document] -> Eval ()

evalTerminal TerminalPreview docs = do
  let jsonVal = documentsToJSON docs
  liftIO $ BL.putStrLn (AP.encodePretty jsonVal)

evalTerminal (TerminalSave path) docs = do
  let jsonVal = documentsToJSON docs
  liftIO $ BL.writeFile path (AP.encodePretty jsonVal)

-------------------------------------------------------
-- EXPRESIONES
-------------------------------------------------------

evalExp :: Exp -> Eval Value

evalExp (IntExp n) = return (VInt n)
evalExp (FloatExp f) = return (VFloat f)
evalExp (StringExp s) = return (VString s)
evalExp (BoolExpVal b) = return (VBool b)
evalExp NullExp = return VNull

evalExp (AddExp a b) = numOp (+) a b
evalExp (SubExp a b) = numOp (-) a b
evalExp (MulExp a b) = numOp (*) a b
evalExp (DivExp a b) = do
  v2 <- evalExp b
  case v2 of
    VInt 0 -> throwError DivisionByZero
    _ -> numOp div a b

evalExp (JObjectExp fields) = do
  vals <- mapM (\(f,e) -> do v <- evalExp e; return (f,v)) fields
  return (VObject vals)

evalExp (JArrayExp xs) = do
  vals <- mapM evalExp xs
  return (VArray vals)

evalExp _ = throwError TypeError

-------------------------------------------------------
-- BOOL
-------------------------------------------------------

evalBool :: BoolExp -> Document -> Eval Bool

evalBool BTrue _ = return True
evalBool BFalse _ = return False

evalBool (Not b) doc = do
  res <- evalBool b doc
  return (not res)

evalBool (And a b) doc = do
  r1 <- evalBool a doc
  r2 <- evalBool b doc
  return (r1 && r2)

evalBool (Or a b) doc = do
  r1 <- evalBool a doc
  r2 <- evalBool b doc
  return (r1 || r2)

evalBool (Eq a b) doc = do
  v1 <- evalDocExp a doc
  v2 <- evalDocExp b doc
  return (v1 == v2)

evalBool (Neq a b) doc = do
  v1 <- evalDocExp a doc
  v2 <- evalDocExp b doc
  return (v1 /= v2)

evalBool (Gt a b) doc = do
  VInt x <- evalDocExp a doc
  VInt y <- evalDocExp b doc
  return (x > y)

evalBool (Ge a b) doc = do
  VInt x <- evalDocExp a doc
  VInt y <- evalDocExp b doc
  return (x >= y)

evalBool (Lt a b) doc = do
  VInt x <- evalDocExp a doc
  VInt y <- evalDocExp b doc
  return (x < y)

evalBool (Le a b) doc = do
  VInt x <- evalDocExp a doc
  VInt y <- evalDocExp b doc
  return (x <= y)

--evalBool (Exists (VarExp f)) doc =
--  return (f `elem` map fst doc)

evalBool (Exists e) doc =
  catchError
    (evalDocExp e doc >> return True)
    (\_ -> return False)

-------------------------------------------------------
-- HELPERS
-------------------------------------------------------
-------------------------------------------------------
-- HELPERS DE ACTUALIZACION DE ESTADO
-------------------------------------------------------

updateDatabase :: (Database -> Database) -> Eval ()
updateDatabase f = do
  st <- get
  let db = database st
  let newDb = f db
  let newState = st { database = newDb }
  put newState

updateRuntime :: (RuntimeContext -> RuntimeContext) -> Eval ()
updateRuntime f = do
  st <- get
  let rt = runtime st
  let newRuntime = f rt
  let newState = st { runtime = newRuntime }
  put newState

updateViews :: (M.Map ViewName Find -> M.Map ViewName Find) -> Eval ()
updateViews f = do
  st <- get
  let rt = runtime st
  let vs = views rt
  let newViews = f vs
  let newRuntime = rt { views = newViews }
  let newState = st { runtime = newRuntime }
  put newState

updateTimestamps ::
  (M.Map TimestampLabel TimestampSnapshot
   -> M.Map TimestampLabel TimestampSnapshot)
  -> Eval ()
updateTimestamps f = do
  st <- get
  let rt = runtime st
  let ts = timestamps rt
  let newTs = f ts
  let newRuntime = rt { timestamps = newTs }
  let newState = st { runtime = newRuntime }
  put newState
{-
updateNextId :: (Int -> Int) -> Eval ()
updateNextId f = do
  st <- get
  let nid = nextId st
  let newId = f nid
  let newState = st { nextId = newId }
  put newState
-}
updateDatabaseAndNextId :: Database -> Int -> Eval ()
updateDatabaseAndNextId newDb newNextId = do
  st <- get
  let newState =
        st
          { database = newDb
          , nextId = newNextId + 1
          }
  put newState

updateDatabaseNextId :: Eval ()
updateDatabaseNextId = do
  st <- get
  let newState = st { nextId = (nextId st) + 1}
  put newState
-------------------------------------------------------
-- VALIDACION DE _id (REUTILIZABLE)
-------------------------------------------------------

-- | Verifica que el documento NO tenga el campo "_id".
-- | Si lo tiene -> Nothing
-- | Si no lo tiene -> Just doc
validateNoIdField :: Document -> Maybe Document
validateNoIdField doc =
  case lookup "_id" doc of
    Just _ -> Nothing
    Nothing -> Just doc

updateDocs :: Bool -> BoolExp -> Document -> [Document] -> Eval [Document]
updateDocs _ _ _ [] = return []
updateDocs stopAfterFirst cond cleanDoc (d:ds) = do
  match <- evalBool cond d
  if match
    then do
      let oldId = getId d
      let merged =
            ("id", oldId) :
            mergeFields
              (filter (\(k,_) -> k /= "_id") d)
              cleanDoc
      if stopAfterFirst
        then return (merged : ds)
        else do
          rest <- updateDocs stopAfterFirst cond cleanDoc ds
          return (merged : rest)

    else do
      rest <- updateDocs stopAfterFirst cond cleanDoc ds
      return (d : rest)



valueToJSON :: Value -> A.Value
valueToJSON (VString s) = A.String (T.pack s)
valueToJSON (VBool b) = A.Bool b
valueToJSON VNull = A.Null
valueToJSON (VInt i) = A.Number (fromIntegral i)
valueToJSON (VFloat f) = A.Number (realToFrac f)

valueToJSON (VArray xs) =
  A.Array (V.fromList (map valueToJSON xs))

valueToJSON (VObject fields) =
  let pairs =
        [ (K.fromText (T.pack k), valueToJSON v)
        | (k,v) <- fields
        ]
  in A.Object (KM.fromList pairs)



documentsToJSON :: [Document] -> A.Value
documentsToJSON docs =
  let jsonDocs =
        map (\doc -> valueToJSON (VObject doc)) docs
  in A.Array (V.fromList jsonDocs)

evalDocExp :: Exp -> Document -> Eval Value

evalDocExp (VarExp f) doc =
  case lookup f doc of
    Just v  -> return v
    Nothing -> throwError (FieldNotFoundInObject f)

evalDocExp (FieldAccess e f) doc = do
  v <- evalDocExp e doc
  case v of
    VObject obj ->
      case lookup f obj of
        Just v2 -> return v2
        Nothing -> throwError (FieldNotFoundInObject f)
    _ -> throwError TypeError

evalDocExp (IntExp n) _ = return (VInt n)
evalDocExp (StringExp s) _ = return (VString s)
evalDocExp (BoolExpVal b) _ = return (VBool b)
evalDocExp NullExp _ = return VNull


evalExpAsDoc :: Exp -> Eval Document
evalExpAsDoc e = do
  v <- evalExp e
  case v of
    VObject obj -> return obj
    _ -> throwError TypeError


selectFields :: [FieldName] -> Document -> Document
selectFields fs doc =
  filter (\(f,_) -> f `elem` fs) doc


numOp :: (Int -> Int -> Int) -> Exp -> Exp -> Eval Value
numOp f a b = do
  VInt x <- evalExp a
  VInt y <- evalExp b
  return (VInt (f x y))

updateOneDoc :: BoolExp -> Document -> [Document] -> Eval [Document]
updateOneDoc _ _ [] = return []

updateOneDoc cond cleanDoc (d:ds) = do
  match <- evalBool cond d
  if match
     then do
       let oldId = getId d
       let merged =
             ("_id", oldId) :
             mergeFields
               (filter (\(k,_) -> k /= "_id") d)
               cleanDoc

       return (merged : ds)

     else do
       rest <- updateOneDoc cond cleanDoc ds
       return (d : rest)

getId :: Document -> Value
getId doc =
  case lookup "_id" doc of
    Just v  -> v
    Nothing -> VNull


mergeFields :: Document -> Document -> Document
mergeFields old new =
  let newKeys = map fst new
  in filter (\(k,_) -> k `notElem` newKeys) old ++ new

getTerminal :: Find -> QueryTerminal
getTerminal (Find _ _ t) = t


getOps :: Find -> [QueryOp]
getOps (Find _ ops _) = ops
