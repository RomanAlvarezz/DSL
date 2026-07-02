module Evaluator where

import AST
import Data.List (groupBy, sortOn, sortBy)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V

import Control.Exception (catch, IOException)

import Control.Monad (ap, liftM, filterM, foldM)

import Value ( Value(..), TimestampSnapshot(..), Document, Database, CollectionData)
import JSONAdapter (valueToJson, databaseToJsonSnap, jsonToDatabaseSnap, timestampSnapshotToJson, jsonToTimestampSnapshot, jsonToFind, findToJson)

-------------------------------------------------------
-- ESTADO GLOBAL DEL EVALUADOR
-------------------------------------------------------

data EvalState = EvalState
  { database :: Database
  , nextId   :: Int
  , logs     :: (Int, [Collection])  -- (cantidadDocumentosModificados, listaDeColeccionesModificadas)
  }

-------------------------------------------------------
-- MONADA PROPIA
-------------------------------------------------------

newtype Eval a = Eval { runEval :: EvalState -> IO (Either EvalError (a, EvalState)) }


{-
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
-}

instance Functor Eval where
  fmap f m = Eval (\s -> do
    res <- runEval m s
    case res of
      Left e -> return (Left e)
      Right (a, s') -> return (Right (f a, s'))
    )

instance Applicative Eval where
  pure x = Eval (\s -> return (Right (x, s)))

  mf <*> ma = Eval (\s -> do
    resF <- runEval mf s
    case resF of
      Left e -> return (Left e)

      Right (f, s') -> do
        resA <- runEval ma s'
        case resA of
          Left e -> return (Left e)
          Right (a, s'') -> return (Right (f a, s''))
    )

instance Monad Eval where
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
  doIoOperation :: IO a -> m a 

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
  doIoOperation io = Eval (\s -> do
    a <- io
    return (Right (a, s)))

-------------------------------------------------------
-- FUNCIONES PURAS,USO EN LAS INSTANCIAS Y DIRECT EN IO
-------------------------------------------------------

lookupMap :: (Eq k) => k -> [(k,v)] -> Maybe v
lookupMap _ [] = Nothing
lookupMap key ((k,v):xs) = if key == k then Just v else lookupMap key xs


memberMap :: (Eq k) => k -> [(k,v)] -> Bool
memberMap key xs = case lookupMap key xs of
                      Just _  -> True
                      Nothing -> False

insertMap :: (Eq k) => k -> v -> [(k,v)] -> [(k,v)]
insertMap key val [] = [(key, val)]
insertMap key val ((k,v):xs) = if key == k then (key, val) : xs else (k,v) : insertMap key val xs


deleteMap :: (Eq k) => k -> [(k,v)] -> [(k,v)]
deleteMap _ [] = []
deleteMap key ((k,v):xs) = if key == k then xs else (k,v) : deleteMap key xs


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
  "El tipo de rollback no coinzzzzcide con el timestamp"

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
  exists <- memberDB name
  if exists
    then throwEval (CollectionAlreadyExists name)
    else do
      insertDB name []
      registerCollectionChange name

evalComm (CommDropColl name) = do
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
  obj <- evalJsonExpAsObject exp emptyDoc
  case validateNoIdField obj of
    Nothing -> throwEval ReservedField
    Just _ -> return ()
  docsMaybe <- lookupDB coll
  case docsMaybe of
    Nothing -> throwEval (CollectionNotFound coll)
    Just docs -> do
      newId <- incId
      let docWithId = ("_id", VNum (NInt newId)) : obj
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
  docsMaybe <- lookupDB coll
  case docsMaybe of
    Nothing -> throwEval (CollectionNotFound coll)
    Just docs -> do
      docs' <- filterM (deleteAux cond) docs
      let deleted = length docs - length docs'
      if deleted > 0
        then do
          insertDB coll docs'
          incDocs deleted
          registerCollectionChange coll
        else
          return ()

-------------------------------------------------------
-- UPDATE
-------------------------------------------------------

evalComm (CommUpdate coll cond exp) = do
  obj <- evalJsonExpAsObject exp emptyDoc
  case validateNoIdField obj of
    Nothing -> throwEval ReservedField
    Just cd -> return cd
  docsMaybe <- lookupDB coll
  case docsMaybe of
    Nothing -> throwEval (CollectionNotFound coll)
    Just docs -> do
      (changed, docs') <- updateDocs cond obj docs
      insertDB coll docs'
      if changed > 0 then do
        incDocs changed
        registerCollectionChange coll
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
  viewsMap <- doIoOperation readViewsFile -- viewsMap tiene una lista con todas las vistas tipo  [ ("mayores", Find ...), ("empleados", Find ...), ...]
  if memberMap name viewsMap
    then throwEval (ViewAlreadyExists name)
    else doIoOperation (writeViewsFile (insertMap name find viewsMap))

evalComm (CommUseView name ViewOnly) = do
  viewsMap <- doIoOperation readViewsFile
  case lookupMap name viewsMap of
    Nothing -> throwEval (ViewNotFound name)
    Just f -> evalComm (CommQuery f)

evalComm (CommUseView name (ViewWithPipeline f)) = do
  viewsMap <- doIoOperation readViewsFile
  case lookupMap name viewsMap of
    Nothing -> throwEval (ViewNotFound name)
    Just (Find coll ops _) ->
      evalComm (CommQuery (Find coll (ops ++ getOps f) (getTerminal f)))


-------------------------------------------------------
-- TIMESTAMPS
-------------------------------------------------------

evalComm (CommTimestamp TSDatabase label) = do
    st <- getEval
    let timestp = DBSnapshot (database st)
    storeTimestamp label timestp


evalComm (CommTimestamp (TSColl coll) label) = do
    docsMaybe <- lookupDB coll
    docs <- case docsMaybe of
        Nothing -> throwEval (CollectionNotFound coll)
        Just d  -> return d
    let timestp = CollSnapshot coll docs
    storeTimestamp label timestp


-------------------------------------------------------
-- ROLLBACK
-------------------------------------------------------
evalComm (CommRollback TSDatabase label) = do
  st <- getEval
  timestmp <- getTimestamp label
  case timestmp of
    DBSnapshot db' -> putEval st { database = db' }
    _              -> throwEval InvalidTimestampTarget

evalComm (CommRollback (TSColl coll) label) = do
  st <- getEval
  timestmp <- getTimestamp label
  case timestmp of
    CollSnapshot coll' docs
      | coll == coll' ->
          let db' = insertMap coll docs (database st)
          in putEval st { database = db' }
    _ -> throwEval InvalidTimestampTarget

-------------------------------------------------------
-- TRANSACCIONES
-------------------------------------------------------
evalComm (CommTransaction comms) = do
  s <- getEval
  catchEval
    (mapM_ evalComm comms)
    (\_ -> putEval s)

-------------------------------------------------------
-- EVALUACION DE CONSULTAS
-------------------------------------------------------
evalFind :: (MonadStateEval m, MonadErrorEval m) => Find -> m [Document]
evalFind (Find coll ops term) = do
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

buildGroupDoc :: (MonadErrorEval m) => [FieldName] -> [Aggregate] -> [Document] -> m Document
buildGroupDoc fields aggs docs = do
  let keyVals = map (\f -> (f, lookupField f (head docs))) fields
  aggResults <- mapM (`applyAggregate` docs) aggs
  return (keyVals ++ aggResults)

applyAggregate :: (MonadErrorEval m) => Aggregate -> [Document] -> m (FieldName, Value)
applyAggregate (Aggregate AggCount _ alias) docs =
  return (alias, VNum (NInt (length docs)))

applyAggregate (Aggregate AggSum path alias) docs = do
  vals <- mapM
    (\d ->
      catchEval
        (evalPathExp (PVar path) d)
        (\_ -> return (VNum (NInt 0)))
    )
    docs

  let nums = [ n | VNum n <- vals ]

  if null nums
    then return (alias, VNull)
    else do
      total <- foldM (numBinaryOp (+) (+)) (NInt 0) nums
      return (alias, VNum total)

applyAggregate (Aggregate AggAvg path alias) docs = do
  vals <- mapM (\d -> catchEval (evalPathExp (PVar path) d) (\_ -> return VNull)) docs
  let nums = [ n | VNum n <- vals ]
  let c = length nums
  
  if c == 0
    then return (alias, VNull)
    else do
      total <- foldM (numBinaryOp (+) (+)) (NInt 0) nums
      -- Convertimos a Float para la división
      let totalValue = case total of
                         NInt i   -> fromIntegral i
                         NFloat f -> f
      return (alias, VNum (NFloat (totalValue / fromIntegral c)))

-- aprovecamos que Value es instancia de Ord para max y min
applyAggregate (Aggregate aggOp path alias) docs = do
  vals <- mapM (\d -> catchEval (evalPathExp (PVar path) d) (\_ -> return VNull)) docs
  let validVals = filter (/= VNull) vals
  
  if null validVals
    then return (alias, VNull)
    else let res = case aggOp of
                     AggMin -> minimum validVals
                     AggMax -> maximum validVals
--                     _      -> VNull -- No debería pasar
         in return (alias, res)

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
  groupedDocs <- mapM (buildGroupDoc fields aggs) groups
  case having of
    Nothing -> return groupedDocs
    Just cond -> filterM (safeEvalBool cond) groupedDocs

-------------------------------------------------------
-- TERMINALES
-------------------------------------------------------
evalTerminal :: (MonadIOEval m) => QueryTerminal -> [Document] -> m ()
evalTerminal TerminalPreview docs = do
  let jsonVal = A.Array (V.fromList (map (valueToJson . VObject) docs))
  doIoOperation (BL.putStrLn (AP.encodePretty jsonVal))

evalTerminal (TerminalSave path) docs = do
  let jsonVal = A.Array (V.fromList (map (valueToJson . VObject) docs))
  doIoOperation (BL.writeFile path (AP.encodePretty jsonVal))

-------------------------------------------------------
-- NUMEXP
-------------------------------------------------------
emptyDoc :: Document
emptyDoc = []

evalNumExp :: (MonadErrorEval m) => NumExp -> Document -> m Number
-- Constantes
evalNumExp (NConst n) _ = return n

-- varibles y acceso a campos numericos
evalNumExp (NPath p) doc = do
  v <- evalPathExp p doc
  case v of
    VNum n -> return n
    _      -> throwEval TypeError

-- Suma
evalNumExp (NAdd a b) doc = do
  n1 <- evalNumExp a doc
  n2 <- evalNumExp b doc
  numBinaryOp (+) (+) n1 n2

-- Resta
evalNumExp (NSub a b) doc = do
  n1 <- evalNumExp a doc
  n2 <- evalNumExp b doc
  numBinaryOp (-) (-) n1 n2

-- Multiplicación
evalNumExp (NMul a b) doc = do
  n1 <- evalNumExp a doc
  n2 <- evalNumExp b doc
  numBinaryOp (*) (*) n1 n2

-- División
evalNumExp (NDiv a b) doc = do
  n1 <- evalNumExp a doc
  n2 <- evalNumExp b doc
  numDivOp n1 n2


-------------------------------------------------------
-- HELPERS NUMERICOS
-------------------------------------------------------
numBinaryOp :: (MonadErrorEval m) => (Int -> Int -> Int) -> (Double -> Double -> Double)
  -> Number -> Number -> m Number

numBinaryOp intOp floatOp (NInt x) (NInt y) =
  return (NInt (intOp x y))

numBinaryOp intOp floatOp (NFloat x) (NFloat y) =
  return (NFloat (floatOp x y))

numBinaryOp intOp floatOp (NInt x) (NFloat y) =
  return (NFloat (floatOp (fromIntegral x) y))

numBinaryOp intOp floatOp (NFloat x) (NInt y) =
  return (NFloat (floatOp x (fromIntegral y)))


numDivOp :: (MonadErrorEval m) => Number -> Number -> m Number
numDivOp _ (NInt 0) =
  throwEval DivisionByZero

numDivOp _ (NFloat 0) =
  throwEval DivisionByZero

numDivOp (NInt x) (NInt y) =
  return (NFloat (fromIntegral x / fromIntegral y)) -- nos permite 5/2 = 2.5

numDivOp (NFloat x) (NFloat y) =
  return (NFloat (x / y))

numDivOp (NInt x) (NFloat y) =
  return (NFloat (fromIntegral x / y))

numDivOp (NFloat x) (NInt y) =
  return (NFloat (x / fromIntegral y))

-------------------------------------------------------
-- STREXP
-------------------------------------------------------
evalStrExp :: (MonadErrorEval m) => StrExp -> Document -> m String
-- Constante string
evalStrExp (SConst s) _ = return s

-- varible y acceso a campos string
evalStrExp (SPath p) doc = do
  v <- evalPathExp p doc
  case v of
    VString s -> return s
    _         -> throwEval TypeError

-------------------------------------------------------
-- BOOLEXP
-------------------------------------------------------
evalBoolExp :: (MonadErrorEval m) => BoolExp -> Document -> m Bool
evalBoolExp BTrue _ = return True
evalBoolExp BFalse _ = return False

-- varibles y acceso a campos booleanos
evalBoolExp (BPath p) doc = do
  v <- evalPathExp p doc
  case v of
    VBool b -> return b
    _       -> throwEval TypeError

-- Operadores lógicos
evalBoolExp (Not b) doc = do -- solamente en este evalBoolExp se usa doc, en el resto no
  v <- evalBoolExp b doc
  return (not v)

evalBoolExp (And a b) doc = do
  v1 <- evalBoolExp a doc
  v2 <- evalBoolExp b doc
  return (v1 && v2)

evalBoolExp (Or a b) doc = do
  v1 <- evalBoolExp a doc
  v2 <- evalBoolExp b doc
  return (v1 || v2)

-- Igualdad / Desigualdad
-- num
evalBoolExp (EqNum a b) doc = do
  v1 <- evalNumExp a doc
  v2 <- evalNumExp b doc
  return (v1 == v2)

evalBoolExp (NeqNum a b) doc = do
  v1 <- evalNumExp a doc
  v2 <- evalNumExp b doc
  return (v1 /= v2)

----- str
evalBoolExp (EqStr a b) doc = do
  v1 <- evalStrExp a doc
  v2 <- evalStrExp b doc
  return (v1 == v2)

evalBoolExp (NeqStr a b) doc = do
  v1 <- evalStrExp a doc
  v2 <- evalStrExp b doc
  return (v1 /= v2)

----- bool
evalBoolExp (EqBool a b) doc = do
  v1 <- evalBoolExp a doc
  v2 <- evalBoolExp b doc
  return (v1 == v2)

evalBoolExp (NeqBool a b) doc = do
  v1 <- evalBoolExp a doc
  v2 <- evalBoolExp b doc
  return (v1 /= v2)

----- null
evalBoolExp (IsNull a) doc = do
  v <- evalPathExp a doc  
  return (v == VNull) 

-------------------------------------------------------
-- Comparaciones numéricas
-------------------------------------------------------
evalBoolExp (Lt a b) doc = do
  n1 <- evalNumExp a doc
  n2 <- evalNumExp b doc
  return (n1 < n2)

evalBoolExp (Le a b) doc = do
  n1 <- evalNumExp a doc
  n2 <- evalNumExp b doc
  return (n1 <= n2)

evalBoolExp (Gt a b) doc = do
  n1 <- evalNumExp a doc
  n2 <- evalNumExp b doc
  return (n1 > n2)

evalBoolExp (Ge a b) doc = do
  n1 <- evalNumExp a doc
  n2 <- evalNumExp b doc
  return (n1 >= n2)

-------------------------------------------------------
-- Exists
-------------------------------------------------------
evalBoolExp (Exists p) doc = 
  catchEval 
    (evalPathExp p doc >> return True) 
    (\e -> case e of
             FieldNotFoundInObject _ -> return False
             _ -> throwEval e
    )

-------------------------------------------------------
-- JSONEXP
-------------------------------------------------------
evalJsonExp :: (MonadErrorEval m) => JsonExp -> Document -> m Value
-- Object
evalJsonExp (JObject fields) doc = do -- ¿Por que JObject pasa a VObject?
  vals <- mapM evalField fields
  return (VObject vals)
  where
    evalField (f, e) = do
      v <- evalJsonExp e doc
      return (f, v)

-- Array
evalJsonExp (JArray xs) doc = do
  vals <- mapM (\e -> evalJsonExp e doc) xs
  return (VArray vals)

-- Num / Str / Bool / Null
evalJsonExp (JNum n) doc = do
  num <- evalNumExp n doc
  return (VNum num)

evalJsonExp (JStr s) doc = do
  str <- evalStrExp s doc
  return (VString str)

evalJsonExp (JBool b) doc = do
  bool <- evalBoolExp b doc
  return (VBool bool)

evalJsonExp JNull _ = return VNull

evalJsonExp (JPath p) doc = evalPathExp p doc

evalPathExp :: (MonadErrorEval m) => PathExp -> Document -> m Value

evalPathExp (PVar f) doc =
  case lookup f doc of
    Just v  -> return v
    Nothing -> throwEval (FieldNotFoundInObject f)

evalPathExp (PAccess f rest) doc =
  case lookup f doc of
    Nothing -> throwEval (FieldNotFoundInObject f)
    Just (VObject obj) -> evalPathExp rest obj
    Just _ -> throwEval TypeError

-------------------------------------------------------
-- HELPERS
-------------------------------------------------------
validateNoIdField :: Document -> Maybe Document
validateNoIdField doc =
  case lookup "_id" doc of
    Just _ -> Nothing
    Nothing -> Just doc

updateDocs :: (MonadErrorEval m) => BoolExp -> Document -> [Document] -> m (Int, [Document])
updateDocs _ _ [] = return (0, [])
updateDocs cond cleanDoc (doc:docs) = do
    (doc', increment) <- processDoc cond cleanDoc doc
    (count, rest) <- updateDocs cond cleanDoc docs
    return (count + increment, doc' : rest)

processDoc :: (MonadErrorEval m) => BoolExp -> Document -> Document -> m (Document, Int)

processDoc cond obj doc = do
    match <- safeEvalBool cond doc
    if not match
      then return (doc, 0)
      else do
          let alreadySame = sameValues obj doc
          if alreadySame
             then return (doc, 0)
             else do
                 let updated = applyChanges doc obj
                 return (updated, 1)

sameValues :: Document -> Document -> Bool
sameValues [] _ = True

sameValues ((field,value):xs) doc =
    case lookup field doc of
      Just v -> if v == value then sameValues xs doc else False
      Nothing -> False

applyChanges :: Document -> Document -> Document
applyChanges [] changes = changes

applyChanges ((field,value):xs) changes =
    case lookup field changes of
        Just newValue -> (field,newValue) : applyChanges xs (filter (\(k,_) -> k /= field) changes)
        Nothing -> (field,value) : applyChanges xs changes

safeEvalBool :: (MonadErrorEval m) => BoolExp -> Document -> m Bool
safeEvalBool cond doc =
  catchEval 
    (evalBoolExp cond doc) 
    (\e -> case e of
             FieldNotFoundInObject _ -> return False
             _ -> throwEval e  -- Si es DivisionByZero o TypeError, explota
    )


evalJsonExpAsObject :: (MonadErrorEval m) => JsonExp -> Document -> m Document
evalJsonExpAsObject e doc = do
  v <- evalJsonExp e doc
  case v of
    VObject obj -> return obj
    _           -> throwEval TypeError


selectFields :: [FieldName] -> Document -> Document
selectFields fs doc =
  filter (\(f,_) -> f `elem` fs) doc


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

-- incrementa la cantidad de documentos modificados
incDocs :: (MonadStateEval m) => Int -> m ()
incDocs n = do
  st <- getEval
  let (d, cs) = logs st
  putEval st { logs = (d + n, cs) }


-- registra coleccion modificada
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
  content <- catch (BL.readFile viewsFile) handler
  case A.decode content of
    Nothing -> return []
    Just (A.Object obj) -> return [ ( T.unpack (K.toText k), find) | (k, v) <- KM.toList obj , Right find <- [jsonToFind v]]
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

deleteAux :: (MonadErrorEval m) => BoolExp -> Document -> m Bool
deleteAux cond d = do
    cumple <- safeEvalBool cond d
    return (not cumple)

storeTimestamp :: (MonadIOEval m) => TimestampLabel -> TimestampSnapshot -> m ()
storeTimestamp label timestamp = doIoOperation (do
    tsMap <- readTimestampsFile
    let newMap = insertMap label timestamp tsMap
    writeTimestampsFile newMap)

getTimestamp :: (MonadIOEval m, MonadErrorEval m) => TimestampLabel -> m TimestampSnapshot
getTimestamp label = do
  tsMap <- doIoOperation readTimestampsFile
  case lookupMap label tsMap of
    Nothing -> throwEval (TimestampNotFound label)
    Just s  -> return s