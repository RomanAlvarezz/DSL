module EvaluatorMM where

import AST
import qualified Data.Map as M
import Data.List (groupBy, sortOn, sortBy)
import Data.Scientific (Scientific, scientific)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V

import Control.Monad (ap, liftM, filterM)

import Value ( Value(..), Document, Database, CollectionData)
import JSONAdapter (valueToJson)


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
  m >>= f = Eval $ \s -> do
    res <- runEval m s
    case res of
      Left e -> return (Left e)
      Right (a, s') -> runEval (f a) s'

-------------------------------------------------------
-- TYPECLASSES
-------------------------------------------------------

class Monad m => MonadStateEval m where
  getEval :: m EvalState
  putEval :: EvalState -> m ()

class Monad m => MonadErrorEval m where
  throwEval :: EvalError -> m a
  catchEval :: m a -> (EvalError -> m a) -> m a

class Monad m => MonadIOEval m where
  liftIOEval :: IO a -> m a

-------------------------------------------------------
-- INSTANCIAS PARA Eval
-------------------------------------------------------

instance MonadStateEval Eval where
  getEval = Eval $ \s -> return (Right (s, s))
  putEval s = Eval $ \_ -> return (Right ((), s))

instance MonadErrorEval Eval where
  throwEval e = Eval $ \_ -> return (Left e)

  catchEval m handler = Eval $ \s -> do
    res <- runEval m s
    case res of
      Left e -> runEval (handler e) s
      Right ok -> return (Right ok)

instance MonadIOEval Eval where
  liftIOEval io = Eval $ \s -> do
    a <- io
    return (Right (a, s))

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
  if M.member name db
    then throwEval (CollectionAlreadyExists name)
    else do
      updateDatabase (M.insert name [])
      registerCollectionChange name

evalComm (CommDropColl name) = do
  st <- getEval
  let db = database st
  if M.member name db
    then do
      updateDatabase (M.delete name)
      registerCollectionChange name
    else throwEval (CollectionNotFound name)

-------------------------------------------------------
-- INSERT
-------------------------------------------------------

evalComm (CommInsert coll exp) = do
  doc <- evalExpAsDoc exp
  case validateNoIdField doc of
    Nothing -> throwEval ReservedField
    Just cleanDoc -> do
      st <- getEval
      let db = database st
      let newId = nextId st
      let docWithId = ("_id", VInt newId) : cleanDoc
      case M.lookup coll db of
        Nothing -> throwEval (CollectionNotFound coll)
        Just docs -> do 
          updateDatabase (M.insert coll (docWithId : docs))
          updateDatabaseNextId
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
  case M.lookup coll (database st) of
    Nothing -> throwEval (CollectionNotFound coll)
    Just docs -> do
      docs' <- filterM (\d -> fmap not (safeEvalBool cond d)) docs
      let deleted = length docs - length docs'
      updateDatabase (M.insert coll docs')
      incDocs deleted
      if deleted > 0 then registerCollectionChange coll else return ()



-------------------------------------------------------
-- UPDATE ONE
-------------------------------------------------------
evalComm (CommUpdateOne coll cond exp) = do
  newDoc <- evalExpAsDoc exp

  case validateNoIdField newDoc of
    Nothing -> throwEval ReservedField
    Just cleanDoc -> do

      st <- getEval

      case M.lookup coll (database st) of
        Nothing -> throwEval (CollectionNotFound coll)

        Just docs -> do
          docs' <- updateDocs True cond cleanDoc docs
          let changed = if docs /= docs' then 1 else 0
          updateDatabase (M.insert coll docs')
          incDocs changed
          if changed == 1 then registerCollectionChange coll else return ()

evalComm (CommUpdateMany coll cond exp) = do
 newDoc <- evalExpAsDoc exp
 case validateNoIdField newDoc of
   Nothing -> throwEval ReservedField
   Just cleanDoc -> do
     st <- getEval

     case M.lookup coll (database st) of
       Nothing -> throwEval (CollectionNotFound coll)

       Just docs -> do
         docs' <- updateDocs False cond cleanDoc docs
         let changed = length [ () | (old,new) <- zip docs docs', old /= new ]
         updateDatabase (M.insert coll docs')

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
-- De esta manera me permite que una vista nueva con el mismo nombre que una existente, la pise
--evalComm (CommCreateView name find) =
--  updateViews (M.insert name find)
evalComm (CommCreateView name find) = do
  st <- getEval
  let vs = views (runtime st)
  if M.member name vs
    then throwEval (ViewAlreadyExists name)
    else updateViews (M.insert name find)


evalComm (CommUseView name ViewOnly) = do
  st <- getEval
  case M.lookup name (views (runtime st)) of
    Nothing -> throwEval (ViewNotFound name)
    Just f  -> evalComm (CommQuery f)

evalComm (CommUseView name (ViewWithPipeline f)) = do
  st <- getEval
  case M.lookup name (views (runtime st)) of
    Nothing -> throwEval (ViewNotFound name)
    Just (Find coll ops _) ->
      evalComm (CommQuery (Find coll (ops ++ getOps f) (getTerminal f)))

-------------------------------------------------------
-- TIMESTAMPS
-------------------------------------------------------

evalComm (CommTimestamp target label) = do
  st <- getEval
  let db = database st
  let rt = runtime st

  snap <- case target of
    TSDatabase ->
      return (DBSnapshot db)

    TSColl coll ->
      case M.lookup coll db of
        Nothing -> throwEval (CollectionNotFound coll)
        Just docs -> return (CollSnapshot coll docs)

  updateTimestamps (M.insert label snap)

-------------------------------------------------------
-- ROLLBACK
-------------------------------------------------------

evalComm (CommRollback target label) = do
  st <- getEval
  let rt = runtime st

  snap <- case M.lookup label (timestamps rt) of
    Nothing -> throwEval (TimestampNotFound label)
    Just s  -> return s

  case (target, snap) of

    (TSDatabase, DBSnapshot db') ->
      putEval st { database = db' }

    (TSColl coll, CollSnapshot c docs)
      | coll == c ->
          let db' = M.insert coll docs (database st)
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
  case M.lookup coll (database st) of
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

--    Just cond ->
--      filterM (evalBool cond) groupedDocs

    Just cond -> filterM (safeEvalBool cond) groupedDocs

-------------------------------------------------------
-- TERMINALES
-------------------------------------------------------
evalTerminal :: (MonadIOEval m) => QueryTerminal -> [Document] -> m ()
evalTerminal TerminalPreview docs = do
  let jsonVal = A.Array (V.fromList (map (valueToJson . VObject) docs))
  liftIOEval $ BL.putStrLn (AP.encodePretty jsonVal)

evalTerminal (TerminalSave path) docs = do
  let jsonVal = A.Array (V.fromList (map (valueToJson . VObject) docs))
  liftIOEval $ BL.writeFile path (AP.encodePretty jsonVal)

-------------------------------------------------------
-- EXPRESIONES
-------------------------------------------------------

evalExp :: (MonadErrorEval m) => Exp -> m Value

evalExp (IntExp n) = return (VInt n)
evalExp (FloatExp f) = return (VFloat f)
evalExp (StringExp s) = return (VString s)
evalExp (BoolExpVal b) = return (VBool b)
evalExp NullExp = return VNull

evalExp (AddExp a b) =
  numOp a b
    (\x y -> return $ VInt (x + y))
    (\x y -> return $ VFloat (x + y))
evalExp (SubExp a b) =
  numOp a b
    (\x y -> return $ VInt (x - y))
    (\x y -> return $ VFloat (x - y))
evalExp (MulExp a b) =
  numOp a b
    (\x y -> return $ VInt (x * y))
    (\x y -> return $ VFloat (x * y))
evalExp (DivExp a b) =
  numOp a b
    (\x y -> if y == 0
                then throwEval DivisionByZero
                else return $ VInt (div x y))
    (\x y -> if y == 0
                then throwEval DivisionByZero
                else return $ VFloat (x / y))

{--
evalExp (AddExp a b) = numOp (+) a b
evalExp (SubExp a b) = numOp (-) a b
evalExp (MulExp a b) = numOp (*) a b
evalExp (DivExp a b) = do
  v2 <- evalExp b
  case v2 of
    VInt 0 -> throwEval DivisionByZero
    _ -> numOp div a b
--}

evalExp (JObjectExp fields) = do
  vals <- mapM (\(f,e) -> do v <- evalExp e; return (f,v)) fields
  return (VObject vals)

evalExp (JArrayExp xs) = do
  vals <- mapM evalExp xs
  return (VArray vals)

evalExp _ = throwEval TypeError

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
  v1 <- evalDocExp a doc
  v2 <- evalDocExp b doc
  return (v1 == v2)

evalBool (Neq a b) doc = do
  v1 <- evalDocExp a doc
  v2 <- evalDocExp b doc
  return (v1 /= v2)

evalBool (Gt a b) doc = do
   v1 <- evalDocExp a doc
   v2 <- evalDocExp b doc
   (a, b) <- conversorFloat v1 v2
   return (a > b)

evalBool (Ge a b) doc = do
  v1 <- evalDocExp a doc
  v2 <- evalDocExp b doc
  (a, b) <- conversorFloat v1 v2
  return (a >= b)

evalBool (Lt a b) doc = do
  v1 <- evalDocExp a doc
  v2 <- evalDocExp b doc
  (a, b) <- conversorFloat v1 v2
  return (a < b)

evalBool (Le a b) doc = do
  v1 <- evalDocExp a doc
  v2 <- evalDocExp b doc
  (a, b) <- conversorFloat v1 v2
  return (a <= b)

evalBool (Exists e) doc =
  catchEval
    (evalDocExp e doc >> return True)
    (\_ -> return False)

-------------------------------------------------------
-- HELPERS
-------------------------------------------------------
-------------------------------------------------------
-- HELPERS DE ACTUALIZACION DE ESTADO
-------------------------------------------------------

updateDatabase :: (MonadStateEval m) => (Database -> Database) -> m ()
updateDatabase f = do
  st <- getEval
  putEval st { database = f (database st) }

updateRuntime :: (MonadStateEval m) => (RuntimeContext -> RuntimeContext) -> m ()
updateRuntime f = do
  st <- getEval
  let rt = runtime st
  let newRuntime = f rt
  putEval st { runtime = newRuntime }

updateViews :: (MonadStateEval m) => (M.Map ViewName Find -> M.Map ViewName Find) -> m ()
updateViews f = do
  st <- getEval
  let rt = runtime st
  let vs = views rt
  let newViews = f vs
  let newRuntime = rt { views = newViews }
  putEval st { runtime = newRuntime }

updateTimestamps :: (MonadStateEval m) => (M.Map TimestampLabel TimestampSnapshot   -> M.Map TimestampLabel TimestampSnapshot) -> m ()
updateTimestamps f = do
  st <- getEval
  let rt = runtime st
  let ts = timestamps rt
  let newTs = f ts
  let newRuntime = rt { timestamps = newTs }
  putEval st { runtime = newRuntime }

updateDatabaseAndNextId :: (MonadStateEval m) => Database -> Int -> m ()
updateDatabaseAndNextId newDb newNextId = do
  st <- getEval
  putEval st
    { database = newDb
    , nextId = newNextId + 1
    }

updateDatabaseNextId :: (MonadStateEval m) => m ()
updateDatabaseNextId = do
  st <- getEval
  putEval st { nextId = nextId st + 1 }
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

evalDocExp :: (MonadErrorEval m) => Exp -> Document -> m Value

evalDocExp (VarExp f) doc =
  case lookup f doc of
    Just v  -> return v
    Nothing -> throwEval (FieldNotFoundInObject f)

evalDocExp (FieldAccess e f) doc = do
  v <- evalDocExp e doc
  case v of
    VObject obj ->
      case lookup f obj of
        Just v2 -> return v2
        Nothing -> throwEval (FieldNotFoundInObject f)
    _ -> throwEval TypeError

evalDocExp (IntExp n) _ = return (VInt n)
evalDocExp (FloatExp n) _ = return (VFloat n)
evalDocExp (StringExp s) _ = return (VString s)
evalDocExp (BoolExpVal b) _ = return (VBool b)
evalDocExp NullExp _ = return VNull

evalDocExp (AddExp a b) _ =
  numOp a b
    (\x y -> return $ VInt (x + y))
    (\x y -> return $ VFloat (x + y))
evalDocExp (SubExp a b) _ =
  numOp a b
    (\x y -> return $ VInt (x - y))
    (\x y -> return $ VFloat (x - y))
evalDocExp (MulExp a b) _ =
  numOp a b
    (\x y -> return $ VInt (x * y))
    (\x y -> return $ VFloat (x * y))
evalDocExp (DivExp a b) _ =
  numOp a b
    (\x y -> if y == 0
                then throwEval DivisionByZero
                else return $ VInt (div x y))
    (\x y -> if y == 0
                then throwEval DivisionByZero
                else return $ VFloat (x / y))
{--
evalDocExp (JObjectExp fields) _ = do
  vals <- mapM (\(f,e) -> do v <- evalExp e; return (f,v)) fields
  return (VObject vals)


evalDocExp (JArrayExp xs) _ = do
  vals <- mapM evalExp xs
  return (VArray vals)
--}

evalExpAsDoc :: (MonadErrorEval m) => Exp -> m Document
evalExpAsDoc e = do
  v <- evalExp e
  case v of
    VObject obj -> return obj
    _ -> throwEval TypeError


selectFields :: [FieldName] -> Document -> Document
selectFields fs doc =
  filter (\(f,_) -> f `elem` fs) doc

numOp :: (MonadErrorEval m) => Exp -> Exp -> (Int -> Int -> m Value) -> (Double -> Double -> m Value) -> m Value
numOp a b intCase floatCase = do
  v1 <- evalExp a
  v2 <- evalExp b
  case (v1, v2) of
    (VInt x, VInt y)     -> intCase x y
    (VFloat x, VFloat y) -> floatCase x y
    _                    -> throwEval TypeError

{--
numOp :: (Int -> Int -> Int) -> Exp -> Exp -> Eval Value
numOp f a b = do
  VInt x <- evalExp a
  VInt y <- evalExp b
  return (VInt (f x y))
--}

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
