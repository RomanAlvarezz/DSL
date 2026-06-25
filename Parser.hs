module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST


totParser :: Parser a -> Parser a
totParser p = do
  whiteSpaceP
  t <- p
  eof
  return t

dsl :: TokenParser ()
dsl = makeTokenParser emptyDef
  { commentLine     = "//"
  , commentStart    = "/*"
  , commentEnd      = "*/"
  , reservedNames =
      [ "find", "filter", "select", "sort", "limit", "createCollection", "dropCollection", "true", "false", "null"
      , "asc", "desc"
      , "insert", "insertMany", "updateOne", "updateMany", "delete"
      , "groupby", "having"
      , "count", "sum", "avg", "min", "max"
      , "preview", "save"
      , "transaction", "timestamp", "rollback"
      , "createView", "useView"
      , "database"
      , "exists", "skip"
      , "eq", "neq", "eqS", "neqS", "eqB", "neqB", "isNull"
      ]
  , reservedOpNames =
      [ ".", ",", ":", "==", "!=", ">", "<", ">=", "<="
      , "+", "-", "*", "/"
      , "&&", "||", "!"
      ,":n",":s",":b",":nl",":p"
      ]
  }

identifierP = identifier dsl
reservedP   = reserved dsl
reservedOpP = reservedOp dsl
parensP     = parens dsl
bracesP     = braces dsl
stringP     = stringLiteral dsl
integerP    = integer dsl
floatP      = float dsl
commaP      = comma dsl
semiP       = semi dsl
whiteSpaceP = whiteSpace dsl

bracketsP :: Parser a -> Parser a
bracketsP = brackets dsl

pipelineKeyword :: String -> Parser ()
pipelineKeyword kw = do
  reservedOpP "."
  reservedP kw

pCollectionPrefix :: String -> Parser Collection
pCollectionPrefix keyword = do
  reservedP keyword
  reservedOpP "."
  identifierP

-- ======================================================
-- PARSER DE NUMEXP
-- ======================================================
pNumExp :: Parser NumExp
pNumExp = parseNumAddSub

parseNumAddSub :: Parser NumExp
parseNumAddSub = chainl1 parseNumMulDiv numAddSubOp

numAddSubOp :: Parser (NumExp -> NumExp -> NumExp)
numAddSubOp = (reservedOpP "+" >> return NAdd) <|> (reservedOpP "-" >> return NSub)

parseNumMulDiv :: Parser NumExp
parseNumMulDiv = chainl1 parseNumFactor numMulDivOp

numMulDivOp :: Parser (NumExp -> NumExp -> NumExp)
numMulDivOp =
      (reservedOpP "*" >> return NMul)
  <|> (reservedOpP "/" >> return NDiv)

parseNumFactor :: Parser NumExp
parseNumFactor =
      parensP pNumExp
  <|> try (do
        f <- floatP
        return (NConst (NFloat f)))
  <|> do
        i <- integerP
        return (NConst (NInt (fromInteger i)))
  <|> do
        name <- pPathExp
        return (NPath name)

-- ======================================================
-- PARSER DE STREXP
-- ======================================================
pStrExp :: Parser StrExp
pStrExp = try pStringConst <|> pStringPath

pStringConst :: Parser StrExp
pStringConst = do
  s <- stringP
  return (SConst s)

pStringPath :: Parser StrExp
pStringPath = do
  name <- pPathExp 
  return (SPath name)

-- ======================================================
-- PARSER DE BOOLEXP
-- ======================================================
pBoolExp :: Parser BoolExp
pBoolExp = parseOr

-- nivel más alto: ||
parseOr :: Parser BoolExp
parseOr = chainl1 parseAnd orOp

orOp :: Parser (BoolExp -> BoolExp -> BoolExp)
orOp = reservedOpP "||" >> return Or

-- nivel medio: &&
parseAnd :: Parser BoolExp
parseAnd = chainl1 parseEqBool andOp

andOp :: Parser (BoolExp -> BoolExp -> BoolExp)
andOp =
  reservedOpP "&&" >> return And

parseEqBool :: Parser BoolExp
parseEqBool = chainl1 parseNot pEqBoolOp

-- nivel siguiente: !
parseNot :: Parser BoolExp
parseNot =
      (reservedOpP "!" >> do
          b <- parseNot
          return (Not b))
  <|> pBoolComparison

-- nivel de comparaciones booleanas, acá viven eqB, eq, eqS y las relacionales
pBoolComparison :: Parser BoolExp
pBoolComparison =
      try pEqNum
  <|> try pEqStr
  <|> try pExists
  <|> try pIsNull
  <|> try pRelational
  <|> try pBoolLiteral
  <|> try pBoolVariable
  <|> parensP pBoolExp


pBoolLiteral :: Parser BoolExp
pBoolLiteral =
      (reservedP "true" >> return BTrue)
  <|> (reservedP "false" >> return BFalse)

pBoolVariable :: Parser BoolExp
pBoolVariable = do
  name <- pPathExp
  return (BPath name)

pExists :: Parser BoolExp
pExists = do
  reservedP "exists"
  p <- parensP pPathExp
  return (Exists p)

pIsNull :: Parser BoolExp
pIsNull = do
  reservedP "isNull"
  field <- parensP pPathExp
  return (IsNull field)

-- comparaciones numéricas
pEqNum :: Parser BoolExp
pEqNum = do
  e1 <- pNumExp
  op <- pEqNumOp
  e2 <- pNumExp
  return (op e1 e2)

pEqNumOp :: Parser (NumExp -> NumExp -> BoolExp)
pEqNumOp =
      (reservedP "eq" >> return EqNum)
  <|> (reservedP "neq" >> return NeqNum)

-- comparaciones de strings
pEqStr :: Parser BoolExp
pEqStr = do
  e1 <- pStrExp
  op <- pEqStrOp
  e2 <- pStrExp
  return (op e1 e2)

pEqStrOp :: Parser (StrExp -> StrExp -> BoolExp)
pEqStrOp =
      (reservedP "eqS" >> return EqStr)
  <|> (reservedP "neqS" >> return NeqStr)



pEqBoolOp :: Parser (BoolExp -> BoolExp -> BoolExp)
pEqBoolOp =
      (reservedP "eqB" >> return EqBool)
  <|> (reservedP "neqB" >> return NeqBool)

-- comparaciones relacionales
pRelational :: Parser BoolExp
pRelational = do
  e1 <- pNumExp
  op <- relOp
  e2 <- pNumExp
  return (op e1 e2)

relOp :: Parser (NumExp -> NumExp -> BoolExp)
relOp =
      (reservedOpP ">=" >> return Ge)
  <|> (reservedOpP "<=" >> return Le)
  <|> (reservedOpP ">"  >> return Gt)
  <|> (reservedOpP "<"  >> return Lt)

-- ======================================================
-- path expressions
-- ======================================================
pPathExp :: Parser PathExp
pPathExp = do
  base <- identifierP
  fields <- many (reservedOpP "." >> identifierP)
  return (buildPath (base : fields))

buildPath :: [FieldName] -> PathExp
buildPath (x:[])   = PVar x
buildPath (x:xs) = PAccess x (buildPath xs)

-- ======================================================
-- PARSER DE JSONEXP
-- ======================================================

pJsonExp :: Parser JsonExp
pJsonExp =
      pJObject
  <|> pJArray
  <|> pJPath


pJObject :: Parser JsonExp
pJObject = do
  fields <- bracesP (pJField `sepBy` commaP)
  return (JObject fields)

pJField :: Parser (FieldName, JsonExp)
pJField =
      try pJFieldNull
  <|> try pJFieldNum
  <|> try pJFieldStr
  <|> try pJFieldBool
  <|> pJFieldPath

pTypedField :: String -> Parser a -> (a -> JsonExp) -> Parser (FieldName, JsonExp)
pTypedField tag valueParser constructor = do
  name <- identifierP
  reservedOpP tag
  value <- valueParser
  return (name, constructor value)

pJFieldNum :: Parser (FieldName, JsonExp)
pJFieldNum = pTypedField ":n" pNumExp JNum

pJFieldStr :: Parser (FieldName, JsonExp)
pJFieldStr = pTypedField ":s" pStrExp JStr

pJFieldBool :: Parser (FieldName, JsonExp)
pJFieldBool = pTypedField ":b" pBoolExp JBool

pConstField :: String -> Parser b -> JsonExp -> Parser (FieldName, JsonExp)
pConstField tag parser result = do
  name <- identifierP
  reservedOpP tag
  parser
  return (name, result)

--pJFieldNull :: Parser (FieldName, JsonExp)
--pJFieldNull = do
--  name <- identifierP
--  reservedOpP ":nl"
--  reservedP "null"
--  return (name, JNull)

pJFieldNull :: Parser (FieldName, JsonExp)
pJFieldNull = pConstField ":nl" (reservedP "null") JNull

pJFieldPath :: Parser (FieldName, JsonExp)
pJFieldPath = pTypedField ":p" (try pJObject <|> pJPath) id

pJArray :: Parser JsonExp
pJArray = do
  elems <- bracketsP (pJObject `sepBy` commaP)
  return (JArray elems)

pJPath :: Parser JsonExp
pJPath = do
  path <- pPathExp
  return (JPath path)

-- ======================================================
-- PARSER DE COMANDOS
-- ======================================================

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
  pipelineKeyword "filter"
  b <- parensP pBoolExp
  return (QFilter b)

-- Select
pSelect :: Parser QueryOp
pSelect = do
  pipelineKeyword "select"
  ids <- parensP (identifierP `sepBy1` commaP)
  return (QSelect ids)

-- Sort
pSort :: Parser QueryOp
pSort = do
  pipelineKeyword "sort"
  fields <- parensP (bracesP (pSortField `sepBy1` commaP))
  return (QSort fields)

-- Limit
pLimit :: Parser QueryOp
pLimit = do
  pipelineKeyword "limit"
  n <- parensP integerP
  return (QLimit (fromInteger n))

-- GroupBy + Aggregaciones + Having
pGroup :: Parser QueryOp
pGroup = do
  pipelineKeyword "groupby"
  fields <- parensP (identifierP `sepBy1` commaP)
  aggs <- many (try pAggregate)
  hav <- optionMaybe (try pHaving)
  return (QGroup (GroupSpec fields aggs hav))

pSortField :: Parser (FieldName, SortOrder)
pSortField = do
  f <- identifierP
  reservedOpP ":"
  o <- do
        reservedP "asc"
        return Asc
      <|> do
        reservedP "desc"
        return Desc
  return (f, o)

pAggregate :: Parser Aggregate
pAggregate =
      try pCount
  <|> try pSum
  <|> try pAvg
  <|> try pMin
  <|> try pMax

pAggregateArgs :: Parser (String, FieldName)
pAggregateArgs =
  parensP $ do
    alias <- stringP
    commaP
    field <- identifierP
    return (alias, field)

pAggregateFunc :: String -> AggFunc -> Parser Aggregate
pAggregateFunc keyword aggFunc = do
  pipelineKeyword keyword
  (alias, field) <- pAggregateArgs
  return (Aggregate aggFunc field alias)

pCount :: Parser Aggregate
pCount = pAggregateFunc "count" AggCount

pSum :: Parser Aggregate
pSum = pAggregateFunc "sum" AggSum

pAvg :: Parser Aggregate
pAvg = pAggregateFunc "avg" AggAvg

pMin :: Parser Aggregate
pMin = pAggregateFunc "min" AggMin

pMax :: Parser Aggregate
pMax =  pAggregateFunc "max" AggMax

pHaving :: Parser BoolExp
pHaving = do
  pipelineKeyword "having"
  parensP pBoolExp

-- Terminales
pTerminal :: Parser QueryTerminal
pTerminal = try pPreview <|> pSave

pPreview :: Parser QueryTerminal
pPreview = do
  pipelineKeyword "preview"
  parensP (return ())
  return TerminalPreview

pSave :: Parser QueryTerminal
pSave = do
  pipelineKeyword "save"
  path <- parensP parseJsonPath
  return (TerminalSave path)

parseJsonPath :: Parser JsonPath
parseJsonPath = do
  char '"'
  name <- many1 (alphaNum <|> char '_' <|> char '-')
  string ".json"
  char '"'
  return (name ++ ".json")

pQuery = do
  q <- pFind
  return (CommQuery q)

--Query completa target
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
pSkip :: Parser Comm
pSkip = do
  reservedP "skip"
  return Skip

pCollectionCommand :: String -> (Collection -> Comm) -> Parser Comm
pCollectionCommand keyword constructor = do
  reservedP keyword
  reservedOpP "."
  col <- identifierP
  parensP (return ())
  return (constructor col)

-- crear coleccion
pCreateCollection :: Parser Comm
pCreateCollection = pCollectionCommand "createCollection" CommCreateColl

-- eliminar coleccion
pDropCollection :: Parser Comm
pDropCollection = pCollectionCommand "dropCollection" CommDropColl

-- insert individual, saco pJsonExp lo hago mas restringido
pInsert :: Parser Comm
pInsert = do
  col <- pCollectionPrefix "insert"
  doc <- parensP pJObject
  return (CommInsert col doc)


-- insert many
pInsertManyComm :: Parser Comm
pInsertManyComm = do
  col <- pCollectionPrefix "insertMany"
  docs <- parensP (bracketsP (pJObject `sepBy1` commaP))
  return (CommInsertMany col docs)


pUpdateCommand :: String -> (Collection -> BoolExp -> JsonExp -> Comm) -> Parser Comm
pUpdateCommand keyword constructor = do
  reservedP keyword
  reservedOpP "."
  col <- identifierP

  (cond, doc) <- parensP ( do
    c <- pBoolExp
    commaP
    d <- pJObject
    return (c, d))

  return (constructor col cond doc)

pUpdateOneComm = pUpdateCommand "updateOne" CommUpdateOne

pUpdateManyComm = pUpdateCommand "updateMany" CommUpdateMany

-- delete document
pDeleteComm :: Parser Comm
pDeleteComm = do
  col <- pCollectionPrefix "delete"
  cond <- parensP pBoolExp
  return (CommDelete col cond)

-- transaccion
pTransactionComm :: Parser Comm
pTransactionComm = do
  reservedP "transaction"
  commList <- bracesP (pSingleStatement `sepBy1` semiP)
  return (CommTransaction commList)



pTimestampLike :: String -> (TimestampTarget -> TimestampLabel -> Comm) -> Parser Comm
pTimestampLike keyword constructor = do
  reservedP keyword
  reservedOpP "."
  target <- pTimestampTarget
  label <- parensP stringP
  return (constructor target label)

pTimestampComm = pTimestampLike "timestamp" CommTimestamp

pRollbackComm = pTimestampLike "rollback" CommRollback

-- tipo de timestamp
pTimestampTarget :: Parser TimestampTarget
pTimestampTarget =
      do
        reservedP "database"
        return TSDatabase
  <|> do
        name <- identifierP
        return (TSColl name)

-- creacion de la vista
pCreateViewComm :: Parser Comm
pCreateViewComm = do
  reservedP "createView"
  (name, findQ) <- parensP ( do
    n <- stringP
    commaP
    f <- pFind
    return (n, f))
  return (CommCreateView name findQ)

-- uso de la vista
pUseViewComm :: Parser Comm
pUseViewComm = do
  reservedP "useView"
  viewName <- parensP stringP
  option <- pViewOption viewName
  return (CommUseView viewName option)

-- se ejecuta la vista sola o con pipeline
pViewOption :: ViewName -> Parser ViewOption
pViewOption viewName =
      try (do
        ops <- many1 pQueryOp
        term <- pTerminal
        return (ViewWithPipeline (Find viewName ops term))
      )
  <|> return ViewOnly

pStatement :: Parser Comm
pStatement = chainl1 pSingleStatement seqOp

seqOp :: Parser (Comm -> Comm -> Comm)
seqOp = do
  semiP
  return Seq

pSingleStatement :: Parser Comm
pSingleStatement =
     pSkip
  <|> pTransactionComm
  <|> pCreateCollection
  <|> pDropCollection
  <|> pCreateViewComm
  <|> pUseViewComm
  <|> pTimestampComm
  <|> pRollbackComm
  <|> pInsertManyComm
  <|> pUpdateOneComm
  <|> pUpdateManyComm
  <|> pDeleteComm
  <|> pInsert
  <|> pQuery
--  <|> do
--        q <- pFind
--        return (CommQuery q)
  
-- Programa completo
pProgram :: Parser Comm
pProgram = totParser pStatement
