module EvaluatorMM2 where

import AST
import qualified Data.Map as M
import Data.List (groupBy, sortOn, sortBy)
import Data.Scientific (Scientific, scientific)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V

import Control.Exception (catch, IOException)

import Control.Monad (ap, liftM, filterM)

import Value ( Value(..), TimestampSnapshot(..), Document, Database, CollectionData)
--import JSONAdapter (valueToJson)
import JSONAdapter (valueToJson, databaseToJsonSnap, jsonToDatabaseSnap, timestampSnapshotToJson, jsonToTimestampSnapshot, jsonToFind, findToJson)


-------------------------------------------------------
-- MONADA PROPIA
-------------------------------------------------------

newtype Eval a = Eval {
  runEval :: EvalState -> IO (Either EvalError (a, EvalState))
}

instance Functor Eval where
  fmap = liftM
instance Applicative Eval where
  pure = return
  (<*>) = ap

instance Monad Eval where
  return x = Eval (\s -> return (Right (x, s)))
  m >>= f = Eval (\s -> do
    res <- runEval m s
    case res of
      Left e -> return (Left e)
      Right (a, s') -> runEval (f a) s')

-------------------------------------------------------
-- TYPECLASSES
-------------------------------------------------------

class Monad m => MonadStateEval m where
  getEval :: m EvalState
  putEval :: EvalState -> m ()
  lookupDB  :: Collection -> m (Maybe CollectionData)
  memberDB  :: Collection -> m Bool
  insertDB  :: Collection -> CollectionData -> m ()
  deleteDB  :: Collection -> m ()
  incId :: m Int


class Monad m => MonadErrorEval m where
  throwEval :: EvalError -> m a
  catchEval :: m a -> (EvalError -> m a) -> m a

class Monad m => MonadIOEval m where
  liftIOEval :: IO a -> m a

-------------------------------------------------------
-- INSTANCIAS PARA Eval
-------------------------------------------------------

instance MonadStateEval Eval where
  getEval = Eval (\s -> return (Right (s, s)))

  putEval s = Eval (\_ -> return (Right ((), s)))

  lookupDB coll = Eval (\s ->
    return (Right (lookupMap coll (database s), s)))

  memberDB coll = Eval (\s ->
    return (Right (memberMap coll (database s), s)))

  insertDB coll docs = Eval (\s ->
    let newDb = insertMap coll docs (database s)
    in return (Right ((), s { database = newDb })))

  deleteDB coll = Eval (\s ->
    let newDb = deleteMap coll (database s)
    in return (Right ((), s { database = newDb })))

  incId = Eval (\s ->
    let current = nextId s
        newState = s { nextId = current + 1 }
    in return (Right (current, newState)))


instance MonadErrorEval Eval where
  throwEval e = Eval (\_ -> return (Left e))

  catchEval m handler = Eval (\s -> do
    res <- runEval m s
    case res of
      Left e -> runEval (handler e) s
      Right ok -> return (Right ok))

instance MonadIOEval Eval where
  liftIOEval io = Eval (\s -> do
    a <- io
    return (Right (a, s)))

-------------------------------------------------------
-- FUNCIONES PURAS,USO EN LAS INSTANCIAS Y DIRECT EN IO
-------------------------------------------------------

lookupMap :: (Eq k) => k -> [(k,v)] -> Maybe v
lookupMap _ [] = Nothing
lookupMap key ((k,v):xs) = if key == k then Just v else lookupMap key xs


memberMap :: (Eq k) => k -> [(k,v)] -> Bool
memberMap key xs =
  case lookupMap key xs of
    Just _  -> True
    Nothing -> False


insertMap :: (Eq k) => k -> v -> [(k,v)] -> [(k,v)]
insertMap key val [] = [(key, val)]
insertMap key val ((k,v):xs) = if key == k then (key, val) : xs else (k,v) : insertMap key val xs


deleteMap :: (Eq k) => k -> [(k,v)] -> [(k,v)]
deleteMap _ [] = []
deleteMap key ((k,v):xs) = if key == k then xs else (k,v) : deleteMap key xs


-------------------------------------------------------
-- ESTADO GLOBAL DEL EVALUADOR
-------------------------------------------------------

data EvalState = EvalState
  { database :: Database
  , nextId   :: Int
  , logs     :: (Int, [Collection])  -- cantidadDocumentosModificados, listaDeColeccionesModificadas)
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
  | CollectionAlreadyExists Collection
  | ViewAlreadyExists ViewName
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

showError (CollectionAlreadyExists c) =
  "La colección '" ++ c ++ "' ya existe"

showError (ViewAlreadyExists v) =
  "La vista '" ++ v ++ "' ya existe"


-------------------------------------------------------
-- PROGRAMA
-------------------------------------------------------

evalProgram :: (MonadStateEval m, MonadErrorEval m, MonadIOEval m) => Program -> m ()
evalProgram = evalComm

-------------------------------------------------------
-- COMANDOS
-------------------------------------------------------

evalComm :: (MonadStateEval m, MonadErrorEval m, MonadIOEval m) => Comm -> m ()

evalComm Skip = return ()

evalComm (Seq c1 c2) = do
  evalComm c1
  evalComm c2

-------------------------------------------------------
-- CREATE / DROP
-------------------------------------------------------

evalComm (CommCreateColl name) = do
  st <- getEval
  let db = database st
  exists <- memberDB name
  if exists
    then throwEval (CollectionAlreadyExists name)
    else do
      insertDB name []
      registerCollectionChange name

evalComm (CommDropColl name) = do
  st <- getEval
  let db = database st
  exists <- memberDB name
  if exists
    then do
      deleteDB name
      registerCollectionChange name
    else throwEval (CollectionNotFound name)

-------------------------------------------------------
-- INSERT
-------------------------------------------------------

evalComm (CommInsert coll exp) = do
  doc <- evalExpAsDoc exp Nothing
  case validateNoIdField doc of
    Nothing -> throwEval ReservedField
    Just cleanDoc -> do
      docsMaybe <- lookupDB coll
      case docsMaybe of
        Nothing -> throwEval (CollectionNotFound coll)
        Just docs -> do
          newId <- incId
          let docWithId = ("_id", VInt newId) : cleanDoc
          insertDB coll (docWithId : docs)
          incDocs 1
          registerCollectionChange coll

evalComm (CommInsertMany _ []) = return ()
evalComm (CommInsertMany coll (e:es)) = do
  evalComm (CommInsert coll e)
  evalComm (CommInsertMany coll es)

-------------------------------------------------------
-- DELETE
-------------------------------------------------------

evalComm (CommDelete coll cond) = do
  st <- getEval
  docsMaybe <- lookupDB coll
  case docsMaybe of
    Nothing -> throwEval (CollectionNotFound coll)
    Just docs -> do
      docs' <- filterM (\d -> fmap not (safeEvalBool cond d)) docs
      let deleted = length docs - length docs'
      insertDB coll docs'
      incDocs deleted
      if deleted > 0 then registerCollectionChange coll else return ()



-------------------------------------------------------
-- UPDATE ONE
-------------------------------------------------------
evalComm (CommUpdateOne coll cond exp) = do
  newDoc <- evalExpAsDoc exp Nothing

  case validateNoIdField newDoc of
    Nothing -> throwEval ReservedField
    Just cleanDoc -> do

      st <- getEval

      docsMaybe <- lookupDB coll
      case docsMaybe of
        Nothing -> throwEval (CollectionNotFound coll)

        Just docs -> do
          docs' <- updateDocs True cond cleanDoc docs
          let changed = if docs /= docs' then 1 else 0
          insertDB coll docs'
          incDocs changed
          if changed == 1 then registerCollectionChange coll else return ()

evalComm (CommUpdateMany coll cond exp) = do
 newDoc <- evalExpAsDoc exp Nothing
 case validateNoIdField newDoc of
   Nothing -> throwEval ReservedField
   Just cleanDoc -> do
     st <- getEval

     docsMaybe <- lookupDB coll
     case docsMaybe of
       Nothing -> throwEval (CollectionNotFound coll)

       Just docs -> do
         docs' <- updateDocs False cond cleanDoc docs
         let changed = length [()| (old,new) <- zip docs docs', old /= new ]
         insertDB coll docs'

         incDocs changed

         if changed > 0
            then registerCollectionChange coll
            else return ()


-------------------------------------------------------
-- CONSULTAS
-------------------------------------------------------

evalComm (CommQuery find) = do
  res <- evalFind find
  evalTerminal (getTerminal find) res

-------------------------------------------------------
-- VISTAS
-------------------------------------------------------

evalComm (CommCreateView name find) = do
  viewsMap <- liftIOEval readViewsFile
  if memberMap name viewsMap
    then throwEval (ViewAlreadyExists name)
    else liftIOEval (writeViewsFile (insertMap name find viewsMap))

evalComm (CommUseView name ViewOnly) = do
  viewsMap <- liftIOEval readViewsFile
  case lookupMap name viewsMap of
    Nothing -> throwEval (ViewNotFound name)
    Just f -> evalComm (CommQuery f)

evalComm (CommUseView name (ViewWithPipeline f)) = do
  viewsMap <- liftIOEval readViewsFile
  case lookupMap name viewsMap of
    Nothing -> throwEval (ViewNotFound name)
    Just (Find coll ops _) ->
      evalComm (CommQuery (Find coll (ops ++ getOps f) (getTerminal f)))


-------------------------------------------------------
-- TIMESTAMPS
-------------------------------------------------------
evalComm (CommTimestamp target label) = do
  st <- getEval
  let db = database st

  snap <- case target of
    TSDatabase ->
      return (DBSnapshot db)

    TSColl coll -> do
      docsMaybe <- lookupDB coll
      case docsMaybe of
        Nothing -> throwEval (CollectionNotFound coll)
        Just docs -> return (CollSnapshot coll docs)

  liftIOEval ( do
    tsMap <- readTimestampsFile
    let newMap = insertMap label snap tsMap
    writeTimestampsFile newMap)


-------------------------------------------------------
-- ROLLBACK
-------------------------------------------------------
evalComm (CommRollback target label) = do
  st <- getEval

  tsMap <- liftIOEval readTimestampsFile

  snap <- case lookupMap label tsMap of
    Nothing -> throwEval (TimestampNotFound label)
    Just s  -> return s

  case (target, snap) of

    (TSDatabase, DBSnapshot db') ->
      putEval st { database = db' }

    (TSColl coll, CollSnapshot c docs)
      | coll == c ->
          let db' = insertMap coll docs (database st)
          in putEval st { database = db' }

    _ -> throwEval InvalidTimestampTarget

-------------------------------------------------------
-- TRANSACCIONES
-------------------------------------------------------

evalComm (CommTransaction comms) = do
  snapshot <- getEval
  catchEval
    (mapM_ evalComm comms)
    (\_ -> putEval snapshot)

-------------------------------------------------------
-- EVALUACION DE CONSULTAS
-------------------------------------------------------

evalFind :: (MonadStateEval m, MonadErrorEval m) => Find -> m [Document]
evalFind (Find coll ops term) = do
  st <- getEval
  docsMaybe <- lookupDB coll
  case docsMaybe of
    Nothing -> throwEval (CollectionNotFound coll)
    Just docs -> applyPipeline docs ops


applyPipeline :: (MonadErrorEval m) => [Document] -> [QueryOp] -> m [Document]
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

applyAggregate (Aggregate AggAvg field alias) docs =
  let vals = [fromIntegral n | doc <- docs, Just (VInt n) <- [lookup field doc]] ++  [f | doc <- docs, Just (VFloat f) <- [lookup field doc]]
      c = length vals
  in if c == 0
        then (alias, VNull)
        else
          let avg = sum vals / fromIntegral c
          in (alias, VFloat avg) -- (truncateTo 3 avg)

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

-------------------------------------------------------
-- PIPELINE
-------------------------------------------------------

applyOp :: (MonadErrorEval m) => [Document] -> QueryOp -> m [Document]

applyOp docs (QFilter cond) =
  filterM (safeEvalBool cond) docs

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

  let groupedDocs = map (buildGroupDoc fields aggs) groups

  case having of
    Nothing -> return groupedDocs

    Just cond -> filterM (safeEvalBool cond) groupedDocs

-------------------------------------------------------
-- TERMINALES
-------------------------------------------------------
evalTerminal :: (MonadIOEval m) => QueryTerminal -> [Document] -> m ()
evalTerminal TerminalPreview docs = do
  let jsonVal = A.Array (V.fromList (map (valueToJson . VObject) docs))
  liftIOEval (BL.putStrLn (AP.encodePretty jsonVal))

evalTerminal (TerminalSave path) docs = do
  let jsonVal = A.Array (V.fromList (map (valueToJson . VObject) docs))
  liftIOEval (BL.writeFile path (AP.encodePretty jsonVal))

-------------------------------------------------------
-- EXPRESIONES
-------------------------------------------------------

evalExp :: (MonadErrorEval m) => Exp -> Maybe Document -> m Value

-- Literales
evalExp (IntExp n) _ = return (VInt n)
evalExp (FloatExp f) _ = return (VFloat f)
evalExp (StringExp s) _ = return (VString s)
evalExp (BoolExpVal b) _ = return (VBool b)
evalExp NullExp _ = return VNull

-- Variables (solo si hay documento)
evalExp (VarExp f) (Just doc) =
  case lookup f doc of
    Just v  -> return v
    Nothing -> throwEval (FieldNotFoundInObject f)

evalExp (VarExp _) Nothing =
  throwEval TypeError

-- Acceso a campos
evalExp (FieldAccess e f) env = do
  v <- evalExp e env
  case v of
    VObject obj ->
      case lookup f obj of
        Just v2 -> return v2
        Nothing -> throwEval (FieldNotFoundInObject f)
    _ -> throwEval TypeError

-- Aritmética
evalExp (AddExp a b) env =
  numOp a b env
    (\x y -> return (VInt (x + y)))
    (\x y -> return (VFloat (x + y)))

evalExp (SubExp a b) env =
  numOp a b env
    (\x y -> return (VInt (x - y)))
    (\x y -> return (VFloat (x - y)))

evalExp (MulExp a b) env =
  numOp a b env
    (\x y -> return (VInt (x * y)))
    (\x y -> return (VFloat (x * y)))

evalExp (DivExp a b) env =
  numOp a b env
    (\x y -> if y == 0
                then throwEval DivisionByZero
                else return (VInt (div x y)))
    (\x y -> if y == 0
                then throwEval DivisionByZero
                else return (VFloat (x / y)))

-- Objetos
evalExp (JObjectExp fields) env = do
  vals <- mapM (\(f,e) -> do v <- evalExp e env; return (f,v)) fields
  return (VObject vals)

-- Arrays
evalExp (JArrayExp xs) env = do
  vals <- mapM (\e -> evalExp e env) xs
  return (VArray vals)

-- Catch-all
--evalExp _ _ = throwEval TypeError

-------------------------------------------------------
-- BOOL
-------------------------------------------------------

evalBool :: (MonadErrorEval m) => BoolExp -> Document -> m Bool

evalBool BTrue _ = return True
evalBool BFalse _ = return False

evalBool (Not b) doc = do
  res <- evalBool b doc
  return (not res)

evalBool (And a b) doc = do
  v1 <- evalBool a doc
  v2 <- evalBool b doc
  return (v1 && v2)

evalBool (Or a b) doc = do
  v1 <- evalBool a doc
  v2 <- evalBool b doc
  return (v1 || v2)

evalBool (Eq a b) doc = do
  v1 <- evalExp a (Just doc)
  v2 <- evalExp b (Just doc)
  return (v1 == v2)

evalBool (Neq a b) doc = do
  v1 <- evalExp a (Just doc)
  v2 <- evalExp b (Just doc)
  return (v1 /= v2)

evalBool (Gt a b) doc = do
   v1 <- evalExp a (Just doc)
   v2 <- evalExp b (Just doc)
   (a, b) <- conversorFloat v1 v2
   return (a > b)

evalBool (Ge a b) doc = do
  v1 <- evalExp a (Just doc)
  v2 <- evalExp b (Just doc)
  (a, b) <- conversorFloat v1 v2
  return (a >= b)

evalBool (Lt a b) doc = do
  v1 <- evalExp a (Just doc)
  v2 <- evalExp b (Just doc)
  (a, b) <- conversorFloat v1 v2
  return (a < b)

evalBool (Le a b) doc = do
  v1 <- evalExp a (Just doc)
  v2 <- evalExp b (Just doc)
  (a, b) <- conversorFloat v1 v2
  return (a <= b)

evalBool (Exists e) doc =
  catchEval
    (evalExp e (Just doc) >> return True)
    (\_ -> return False)

-------------------------------------------------------
-- HELPERS
-------------------------------------------------------

-- | Verifica que el documento NO tenga el campo "_id".
-- | Si lo tiene -> Nothing
-- | Si no lo tiene -> Just doc
validateNoIdField :: Document -> Maybe Document
validateNoIdField doc =
  case lookup "_id" doc of
    Just _ -> Nothing
    Nothing -> Just doc

updateDocs :: (MonadErrorEval m) => Bool -> BoolExp -> Document -> [Document] -> m [Document]
updateDocs _ _ _ [] = return []

updateDocs stopAfterFirst cond cleanDoc (d:ds) = do
  match <- safeEvalBool cond d
  if match
    then do
      let alreadySame =
            all (\(k,v) -> lookup k d == Just v) cleanDoc

      if alreadySame
        then
          if stopAfterFirst
            then return (d:ds)
            else do
              rest <- updateDocs stopAfterFirst cond cleanDoc ds
              return (d:rest)

        else do
          let oldId = getId d
          let merged =
                ("_id", oldId) :
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

conversorFloat :: (MonadErrorEval m) => Value -> Value -> m (Double, Double)
conversorFloat (VInt x) (VInt y) = return (fromIntegral x, fromIntegral y)
conversorFloat (VFloat x) (VFloat y) = return (x,  y)
conversorFloat (VInt x) (VFloat y) = return (fromIntegral x, y)
conversorFloat (VFloat x) (VInt y) = return ( x, fromIntegral y)
conversorFloat  _ _ = throwEval TypeError

safeEvalBool :: (MonadErrorEval m) => BoolExp -> Document -> m Bool
safeEvalBool cond doc =
  catchEval (evalBool cond doc) (\_ -> return False)


evalExpAsDoc :: (MonadErrorEval m) => Exp -> Maybe Document -> m Document
evalExpAsDoc e mDoc = do
  v <- evalExp e mDoc
  case v of
    VObject obj -> return obj
    _ -> throwEval TypeError


selectFields :: [FieldName] -> Document -> Document
selectFields fs doc =
  filter (\(f,_) -> f `elem` fs) doc

numOp :: (MonadErrorEval m) => Exp -> Exp -> Maybe Document -> (Int -> Int -> m Value) -> (Double -> Double -> m Value)-> m Value

numOp a b env intCase floatCase = do
  v1 <- evalExp a env
  v2 <- evalExp b env
  case (v1, v2) of
    (VInt x, VInt y)     -> intCase x y
    (VFloat x, VFloat y) -> floatCase x y
    _                    -> throwEval TypeError


updateOneDoc :: (MonadErrorEval m) => BoolExp -> Document -> [Document] -> m [Document]
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

truncateToScientific :: Int -> Double -> Scientific
truncateToScientific n x =
  let factor = 10 ^ n
      scaled = truncate (x * fromIntegral factor)
  in scientific scaled (negate n)

-- | Incrementa la cantidad de documentos modificados
incDocs :: (MonadStateEval m) => Int -> m ()
incDocs n = do
  st <- getEval
  let (d, cs) = logs st
  putEval st { logs = (d + n, cs) }

-------------------------------------------------------
-- REGISTRAR COLECCION MODIFICADA
-------------------------------------------------------

-- | Registra el nombre de una colección modificada.
-- | Si ya está en la lista de colecciones modificadas
-- | no hace nada.
-- | Si no está, la agrega.
registerCollectionChange :: (MonadStateEval m) => Collection -> m ()
registerCollectionChange coll = do
  st <- getEval
  let (d, cs) = logs st
  if coll `elem` cs
     then return ()
     else putEval st { logs = (d, coll:cs) }

-------------------------------------------------------
-- MANEJO DE ARCHIVO DE TIMESTAMPS
-------------------------------------------------------

timestampsFile :: FilePath
timestampsFile = "timestamps.json"

handler :: IOException -> IO BL.ByteString
handler _ = return (BL.pack "{}")

readTimestampsFile :: IO [(TimestampLabel, TimestampSnapshot)]
readTimestampsFile = do
  content <- catch
      (BL.readFile timestampsFile)
      handler

  case A.decode content of
    Nothing -> return []

    Just (A.Object obj) ->
      return
        [ ( T.unpack (K.toText k)
          , snap
          )
        | (k, v) <- KM.toList obj
        , Right snap <- [jsonToTimestampSnapshot v]
        ]

    _ -> return []


writeTimestampsFile :: [(TimestampLabel, TimestampSnapshot)] -> IO ()
writeTimestampsFile tsList = do
  let jsonObj =
        A.Object (
          KM.fromList
            [ ( K.fromText (T.pack label)
              , timestampSnapshotToJson snap
              )
            | (label, snap) <- tsList
            ]
        )

  BL.writeFile timestampsFile (AP.encodePretty jsonObj)


-------------------------------------------------------
-- MANEJO DE ARCHIVO DE VIEWS
-------------------------------------------------------

viewsFile :: FilePath
viewsFile = "views.json"


readViewsFile :: IO [(ViewName, Find)]
readViewsFile = do
  content <- catch
      (BL.readFile viewsFile)
      handler

  case A.decode content of
    Nothing -> return []

    Just (A.Object obj) ->
      return
        [ ( T.unpack (K.toText k)
          , find
          )
        | (k, v) <- KM.toList obj
        , Right find <- [jsonToFind v]
        ]

    _ -> return []


writeViewsFile :: [(ViewName, Find)] -> IO ()
writeViewsFile viewsList = do
  let jsonObj =
        A.Object (
          KM.fromList
            [ ( K.fromText (T.pack name)
              , findToJson find
              )
            | (name, find) <- viewsList
            ]
        )

  BL.writeFile viewsFile (AP.encodePretty jsonObj)
