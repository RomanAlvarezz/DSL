module Parser where

import Control.Monad (guard)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST


-- Lexer

dsl :: TokenParser ()
dsl = makeTokenParser emptyDef
  { commentLine     = "//"
  , commentStart    = "/*"
  , commentEnd      = "*/"
  , reservedNames   =
      [ "find", "true", "false", "null"
      , "asc", "desc"
      , "insert", "insertMany", "updateOne", "delete"
      , "groupby", "having"
      , "preview", "save"
      , "transaction", "timestamp", "rollback"
      , "createView", "useView"
      ]
  , reservedOpNames =
      [ ".", ",", ":", "==", "!=", ">", "<", ">=", "<="
      , "+", "-", "*", "/"
      , "&&", "||"
      ]
  }
-- pArray
identifierP = identifier dsl
reservedP   = reserved dsl
reservedOpP = reservedOp dsl
parensP     = parens dsl
bracesP     = braces dsl
stringP     = stringLiteral dsl
integerP    = integer dsl
floatP      = float dsl
commaP      = comma dsl
whiteSpaceP = whiteSpace dsl

bracketsP :: Parser a -> Parser a
bracketsP = brackets dsl


-- Expresiones group

pExp :: Parser Exp
pExp = buildExpressionParser table pTerm

pTerm :: Parser Exp
pTerm =
      parensP pExp
  <|> try (FloatExp <$> floatP)
  <|> IntExp . fromInteger <$> integerP
  <|> StringExp <$> stringP
  <|> BoolExpVal True  <$ reservedP "true"
  <|> BoolExpVal False <$ reservedP "false"
  <|> NullExp <$ reservedP "null"
  <|> VarExp <$> identifierP
  <|> pObject
  <|> pArray

-- Objetos y Arrays JSON     

pObject :: Parser Exp
pObject = JObjectExp <$> bracesP (pField `sepBy` commaP)

pField :: Parser (FieldName, Exp)
pField = do
  f <- identifierP
  reservedOpP ":"
  v <- pExp
  return (f, v)

pArray :: Parser Exp
pArray = JArrayExp <$> bracketsP (pExp `sepBy` commaP)


-- Operadores aritméticos (precedencia)

table =
  [ [ binary "*" MulExp AssocLeft
    , binary "/" DivExp AssocLeft
    ]
  , [ binary "+" AddExp AssocLeft
    , binary "-" SubExp AssocLeft
    ]
  ]

binary name fun assoc =
  Infix (reservedOpP name >> return fun) assoc

-- Expresiones booleanas

pBoolExp :: Parser BoolExp
pBoolExp = buildExpressionParser boolTable pBoolTerm

pBoolTerm :: Parser BoolExp
pBoolTerm =
      parensP pBoolExp
  <|> pComparison
  <|> BTrue  <$ reservedP "true"
  <|> BFalse <$ reservedP "false"

pComparison :: Parser BoolExp
pComparison = do
  e1 <- pExp
  op <- compOp
  e2 <- pExp
  return (op e1 e2)

compOp =
      Eq  <$ reservedOpP "=="
  <|> Neq <$ reservedOpP "!="
  <|> Gt  <$ reservedOpP ">"
  <|> Ge  <$ reservedOpP ">="
  <|> Lt  <$ reservedOpP "<"
  <|> Le  <$ reservedOpP "<="

boolTable =
  [ [ Prefix (reservedOpP "!" >> return Not) ]
  , [ Infix (reservedOpP "&&" >> return And) AssocLeft ]
  , [ Infix (reservedOpP "||" >> return Or)  AssocLeft ]
  ]

-- Operaciones del pipeline

pQueryOp :: Parser QueryOp
pQueryOp =
      try pFilter
  <|> try pSelect
  <|> try pSort
  <|> try pLimit
  <|> try pGroup

-- Filter

pFilter :: Parser QueryOp
pFilter = do
  reservedOpP "."
  reservedP "filter"
  QFilter <$> parensP pBoolExp

-- Select

pSelect :: Parser QueryOp
pSelect = do
  reservedOpP "."
  reservedP "select"
  QSelect <$> parensP (identifierP `sepBy1` commaP)

-- Sort

pSort :: Parser QueryOp
pSort = do
  reservedOpP "."
  reservedP "sort"
  QSort <$> parensP (bracesP (pSortField `sepBy1` commaP))

pSortField :: Parser (FieldName, SortOrder)
pSortField = do
  f <- identifierP
  reservedOpP ":"
  o <- (Asc <$ reservedP "asc") <|> (Desc <$ reservedP "desc")
  return (f, o)

-- Limit
pLimit :: Parser QueryOp
pLimit = do
  reservedOpP "."
  reservedP "limit"
  QLimit . fromInteger <$> parensP integerP

-- GroupBy + Aggregaciones + Having
pGroup :: Parser QueryOp
pGroup = do
  reservedOpP "."
  reservedP "groupby"
  field <- parensP identifierP
  aggs  <- many pAggregate
  hav   <- optionMaybe pHaving
  return $ QGroup (GroupSpec field aggs hav)

pAggregate :: Parser Aggregate
pAggregate =
      pAgg "count" AggCount
  <|> pAgg "sum"   AggSum
  <|> pAgg "avg"   AggAvg
  <|> pAgg "min"   AggMin
  <|> pAgg "max"   AggMax

pAgg :: String -> AggFunc -> Parser Aggregate
pAgg name f = do
  reservedOpP "."
  reservedP name
  (alias, field) <- parensP $ do
    a <- stringP
    commaP
    b <- identifierP
    return (a, b)
  return (Aggregate f field alias)

pHaving :: Parser BoolExp
pHaving = do
  reservedOpP "."
  reservedP "having"
  parensP pBoolExp

-- Terminales

pTerminal :: Parser QueryTerminal
pTerminal =
      pPreview
  <|> pSave

pPreview :: Parser QueryTerminal
pPreview = do
  reservedOpP "."
  reservedP "preview"
  parensP (return ())
  return TerminalPreview

pSave :: Parser QueryTerminal
pSave = do
  reservedOpP "."
  reservedP "save"
  TerminalSave <$> parensP parseJsonPath

parseJsonPath :: Parser JsonPath
parseJsonPath = do
  path <- stringP
  guard (endsWithJson path) <?> "archivo.json"
  return path

endsWithJson :: String -> Bool
endsWithJson s =
  length s > 5 && drop (length s -5) s == ".json"

--Query completa
pFind :: Parser Find
pFind = do
  reservedP "find"
  reservedOpP "."
  col <- identifierP
  parensP (return ())
  ops <- many pQueryOp
  term <- pTerminal
  return (Find col ops term)

--Statements
pInsert :: Parser Comm
pInsert = do
  reservedP "insert"
  reservedOpP "."
  col <- identifierP
  doc <- parensP pExp
  return (CommInsert col doc)

pStatement :: Parser Comm
pStatement =
      try (CommQuery <$> pFind)
  <|> pInsert

-- Programa completo

pProgram :: Parser Program
pProgram = whiteSpaceP *> many pStatement <* eof
