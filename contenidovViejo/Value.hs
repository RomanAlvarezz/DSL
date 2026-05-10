-- Value.hs
module Value ( Value(..), TimestampSnapshot(..), Document, CollectionData, Database, FieldName, Collection) where
import AST (TimestampLabel)
import qualified Data.Map as M

-------------------------------------------------------
-- TIPOS BASE DEL DOMINIO
-------------------------------------------------------

type FieldName = String
type Collection = String

-------------------------------------------------------
-- VALORES JSON INTERNOS (CORE DEL DSL)
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

-------------------------------------------------------
-- MODELO DE BASE DE DATOS
-------------------------------------------------------

type Document = [(FieldName, Value)]
type CollectionData = [Document]

-- cambio el tipo de database para dejar de trabajar con 'M.insert, loolup, ...'
type Database = [(Collection, CollectionData)]
--type Database = M.Map Collection CollectionData


-------------------------------------------------------
-- MODELO DE TIMESTAMP
-------------------------------------------------------

--type TimestampLabel = String

data TimestampSnapshot
  = DBSnapshot Database
  | CollSnapshot Collection CollectionData
  deriving (Show)
