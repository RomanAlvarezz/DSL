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
  | VNum Number    -- Asumiendo que Number (del AST) ya tiene Ord
  | VString String 
  | VArray [Value] 
  | VObject Document
  deriving (Show, Eq, Ord) -- <--- Agregamos Ord aquí


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
