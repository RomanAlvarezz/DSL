module AST where

-- ======================================================
-- PROGRAMA
-- ======================================================
-- Un programa es una secuencia de comandos ejecutables
-- sobre la base de datos JSON.
-- ====================================================== rollback

type Program = [Comm]

type Collection      = String
type ViewName        = String
type FieldName       = String
type TimestampLabel  = String
type JsonPath        = String

-- ======================================================
-- COMANDOS (STATEMENTS)
-- ======================================================
-- Representan las instrucciones de primer nivel del DSL:
-- consultas, inserciones, borrados, transacciones, etc.
-- ======================================================

data Comm
  = CommQuery Find
    -- ^ Ejecución de una consulta (find + pipeline + terminal)

  | CommInsert Collection Exp
    -- ^ Inserta un documento en una colección

  | CommInsertMany Collection [Exp]
    -- ^ Inserta múltiples documentos

  | CommUpdateOne Collection BoolExp Exp
    -- ^ Actualiza el primer documento que cumple la condición

  | CommDelete Collection BoolExp
    -- ^ Elimina documentos que cumplen la condición

  | CommTransaction [Comm]
    -- ^ Bloque transaccional

  | CommTimestamp TimestampTarget TimestampLabel
    -- ^ Marca un estado de la base de datos o una vista

  | CommRollback TimestampLabel
    -- ^ Revierte la base al timestamp indicado

  | CommCreateView ViewName Find
    -- ^ Crea una vista a partir de una consulta

  | CommUseView ViewName Find
    -- ^ Usa una vista como fuente de una consulta
  deriving (Show, Eq)

-- ======================================================
-- CONSULTAS
-- ======================================================
-- Una consulta está compuesta por:
--   - una fuente (colección o vista)
--   - una secuencia de operaciones
--   - un operador terminal obligatorio
-- ======================================================

--data Find = Find
--  { findSource   :: Collection
--  , findOps      :: [QueryOp]
--  , findTerminal :: QueryTerminal
-- }
data Find = Find Collection [QueryOp] QueryTerminal
  deriving (Show, Eq)

-- ======================================================
-- OPERACIONES DE CONSULTA (PIPELINE)
-- ======================================================
-- Representan transformaciones puras sobre el flujo
-- de documentos.
-- ======================================================

data QueryOp
  = QFilter BoolExp
    -- ^ Filtrado de documentos

  | QSelect [FieldName]
    -- ^ Proyección de campos

  | QSort [(FieldName, SortOrder)]
    -- ^ Ordenamiento (multi-campo permitido)

  | QLimit Int
    -- ^ Límite de resultados

  | QGroup GroupSpec
    -- ^ Agrupamiento con agregaciones y having
  deriving (Show, Eq)

-- ======================================================
-- TERMINALES DE CONSULTA
-- ======================================================
-- Indican cómo finaliza una consulta.
-- Solo uno es permitido por consulta.
-- ======================================================

data QueryTerminal
  = TerminalPreview
    -- ^ Muestra el resultado en consola

  | TerminalSave JsonPath
    -- ^ Guarda el resultado en un archivo JSON
  deriving (Show, Eq)

-- ======================================================
-- GROUP BY + AGREGACIONES
-- ======================================================
-- Se agrupan los documentos y se aplican funciones
-- de agregación. Having es opcional.
-- ======================================================

--data GroupSpec = GroupSpec
--  { groupField :: FieldName
--  , aggregates :: [Aggregate]
--  , havingCond :: Maybe BoolExp
--  }
--  deriving (Show, Eq)
data GroupSpec = GroupSpec FieldName [Aggregate] (Maybe BoolExp) deriving (Show, Eq)

--data Aggregate = Aggregate
--  { aggFunc  :: AggFunc
--  , aggField :: FieldName
--  , aggAlias :: FieldName
--  }
--  deriving (Show, Eq)
data Aggregate = Aggregate AggFunc FieldName FieldName deriving (Show, Eq)

data AggFunc
  = AggCount
  | AggSum
  | AggAvg
  | AggMin
  | AggMax
  deriving (Show, Eq)

-- ======================================================
-- EXPRESIONES BOOLEANAS
-- ======================================================
-- Usadas principalmente en filtros y having.
-- ======================================================

data BoolExp
  = BTrue
  | BFalse
  | Not BoolExp
  | And BoolExp BoolExp
  | Or  BoolExp BoolExp
  | Eq  Exp Exp
  | Neq Exp Exp
  | Lt  Exp Exp
  | Le  Exp Exp
  | Gt  Exp Exp
  | Ge  Exp Exp
  deriving (Show, Eq)

-- ======================================================
-- EXPRESIONES
-- ======================================================
-- Representan valores, referencias a campos,
-- operaciones aritméticas y estructuras JSON.
-- ======================================================

data Exp
  = IntExp Int
  | FloatExp Double
  | StringExp String
  | BoolExpVal Bool
  | NullExp

  | AddExp Exp Exp
  | SubExp Exp Exp
  | MulExp Exp Exp
  | DivExp Exp Exp

  | VarExp FieldName
    -- ^ Referencia a un campo del documento

  | FieldAccess Exp FieldName
    -- ^ Acceso a campos anidados

  | JObjectExp [(FieldName, Exp)]
    -- ^ Documento JSON

  | JArrayExp [Exp]
    -- ^ Array JSON

  deriving (Show, Eq)

-- ======================================================
-- AUXILIARES
-- ======================================================

data SortOrder = Asc | Desc
  deriving (Show, Eq)

data TimestampTarget
  = TSDatabase
    -- ^ Timestamp de toda la base de datos

  | TSView ViewName
    -- ^ Timestamp de una vista específica
  deriving (Show, Eq)
