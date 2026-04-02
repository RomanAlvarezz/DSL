-- Value.hs
module Value ( Value(..), Document, CollectionData, Database, FieldName, Collection) where

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
type Database = M.Map Collection CollectionData
