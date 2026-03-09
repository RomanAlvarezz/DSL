module Evaluator where

import AST

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Aeson
import qualified Data.ByteString.Lazy as B



type Document = [(FieldName, Value)]

type CollectionData = [Document]

type Database = Map Collection CollectionData


data Snapshot = DBSnapshot Database | CollSnapshot Collection CollectionData 
deriving (Show)


data RuntimeContext = RuntimeContext
  { views :: Map ViewName Find
  , timestamps :: Map TimestampLabel Snapshot
  }

-- Estado completo del evaluador
data EvalState = EvalState 
  { 
  persistentDB :: Database,
  runtime :: RuntimeContext
  }

-- Estado inicial
emptyRuntime :: RuntimeContext
emptyRuntime =
  RuntimeContext
    { views = Map.empty
    , timestamps = Map.empty
    }

emptyState :: Database -> EvalState
emptyState db =
  EvalState
    { persistentDB = db
    , runtime = emptyRuntime
    }


-- Tipo de la monada de nuestro evaluador
type Eval a = StateT EvalState (ExceptT EvalError IO) a


data Value
= VInt Int
| VFloat Double
| VString String
| VBool Bool
| VNull
| VArray [Value]
| VObject [(FieldName, Value)]
deriving (Show, Eq)

data EvalError
= CollectionNotFound Collection
| ViewNotFound ViewName
| TimestampNotFound TimestampLabel
| InvalidRollback
| RuntimeError String
deriving (Show)
