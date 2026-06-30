module AST where

type Collection      = String
type ViewName        = String
type FieldName       = String
type TimestampLabel  = String
type JsonPath        = String

-- ======================================================
-- COMANDOS (STATEMENTS)
-- ======================================================
data Comm
  = Skip
  | Seq Comm Comm
  -- consultas
  | CommQuery Find
  -- collections
  | CommCreateColl Collection
  | CommDropColl Collection
  -- inserts
  | CommInsert Collection JsonExp
  | CommInsertMany Collection [JsonExp]
  -- update
  | CommUpdate Collection BoolExp JsonExp
  | CommDelete Collection BoolExp
  -- transacciones
  | CommTransaction [Comm]
  -- timestamps
  | CommTimestamp TimestampTarget TimestampLabel
  | CommRollback TimestampTarget TimestampLabel
  -- views
  | CommCreateView ViewName Find
  | CommUseView ViewName ViewOption
  deriving (Show, Eq)

-- ======================================================
-- CONSULTAS
-- ======================================================

data Find = Find Collection [QueryOp] QueryTerminal 
  deriving (Show, Eq)

-- ======================================================
-- OPERACIONES DE CONSULTA (PIPELINE)
-- ======================================================

data QueryOp
  = QFilter BoolExp
  | QSelect [FieldName]
  | QSort [(FieldName, SortOrder)]
  | QLimit Int
  | QGroup GroupSpec
  deriving (Show, Eq)

data SortOrder = Asc | Desc
  deriving (Show, Eq)

-- ======================================================
-- TERMINALES DE CONSULTA
-- ======================================================

data QueryTerminal
  = TerminalPreview
  | TerminalSave JsonPath
  deriving (Show, Eq)

-- ======================================================
-- GROUP BY + AGREGACIONES
-- ======================================================

data GroupSpec = GroupSpec [FieldName] [Aggregate] (Maybe BoolExp)
  deriving (Show, Eq)

data Aggregate = Aggregate AggFunc FieldName FieldName
  deriving (Show, Eq)

data AggFunc
  = AggCount
  | AggSum
  | AggAvg
  | AggMin
  | AggMax
  deriving (Show, Eq)

-- ======================================================
-- TIMESTAMPS
-- ======================================================

data TimestampTarget
  = TSDatabase  -- Timestamp de toda la base de datos
  | TSColl Collection  -- Timestamp de una collection específica
  deriving (Show, Eq)

-- ======================================================
-- VIEWS
-- ======================================================

data ViewOption
  = ViewOnly
  | ViewWithPipeline Find
  deriving (Show, Eq)

-- ======================================================
-- EXPRESIONES
-- ======================================================
data Number = NInt Int | NFloat Double
  deriving (Show, Eq)

instance Ord Number where
  compare (NInt a)   (NInt b)   = compare a b
  compare (NFloat a) (NFloat b) = compare a b
  compare (NInt a)   (NFloat b) = compare (fromIntegral a) b
  compare (NFloat a) (NInt b)   = compare a (fromIntegral b)

-- ======================================================
-- EXPRESIONES NUMERICAS
-- ======================================================
data NumExp
  = NConst Number
  | NPath PathExp
  | NAdd NumExp NumExp
  | NSub NumExp NumExp
  | NMul NumExp NumExp
  | NDiv NumExp NumExp
  deriving (Show, Eq)

-- ======================================================
-- EXPRESIONES STRING
-- ======================================================
data StrExp
  = SConst String
  | SPath PathExp
  deriving (Show, Eq)

-- ======================================================
-- BOOLEANOS
-- ======================================================
data BoolExp
  = BTrue
  | BFalse
  | BPath PathExp
  | Not BoolExp
  | And BoolExp BoolExp
  | Or  BoolExp BoolExp

  | EqNum NumExp NumExp
  | NeqNum NumExp NumExp
  | EqStr StrExp StrExp
  | NeqStr StrExp StrExp
  | EqBool BoolExp BoolExp
  | NeqBool BoolExp BoolExp
  | IsNull PathExp

  | Lt  NumExp NumExp
  | Le  NumExp NumExp
  | Gt  NumExp NumExp
  | Ge  NumExp NumExp

  | Exists PathExp
  deriving (Show, Eq)
  
-- ======================================================
-- PATH (acceso a variables y campos anidados)
-- ======================================================
data PathExp
  = PVar FieldName
  | PAccess FieldName PathExp 
  deriving (Show, Eq)

-- direccion.calle.numero
-- PAccess "direccion" (PAccess "calle" (PVar "numero"))  

-- ======================================================
-- EXPRESIONES JSON
-- ======================================================

data JsonExp
  = JObject [(FieldName, JsonExp)]
  | JArray [JsonExp]
  | JNum NumExp
  | JStr StrExp
  | JBool BoolExp
  | JNull
  | JPath PathExp
  deriving (Show, Eq)