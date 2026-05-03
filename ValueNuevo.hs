module ValueNuevo ( Value(..), TimestampSnapshot(..), Document, CollectionData, Database, FieldName, Collection) where
import ASTNuevo (Number(..), TimestampLabel)
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
  = VNull
  | VBool Bool
  | VNum Number
  | VString String
  | VArray [Value]
  | VObject Document
  deriving (Show, Eq, Ord)


-------------------------------------------------------
-- MODELO DE BASE DE DATOS
-------------------------------------------------------
type Document = [(FieldName, Value)]
type CollectionData = [Document]

type Database = [(Collection, CollectionData)]


-------------------------------------------------------
-- MODELO DE TIMESTAMP
-------------------------------------------------------
data TimestampSnapshot
  = DBSnapshot Database
  | CollSnapshot Collection CollectionData
  deriving (Show)
