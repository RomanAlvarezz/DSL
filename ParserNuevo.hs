module ParserNuevo where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import ASTNuevo


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

-- ======================================================
-- PARSER DE NUMEXP
-- ======================================================

pNumExp :: Parser NumExp
pNumExp = parseNumAddSub

parseNumAddSub :: Parser NumExp
parseNumAddSub = chainl1 parseNumMulDiv numAddSubOp

numAddSubOp :: Parser (NumExp -> NumExp -> NumExp)
numAddSubOp =
      (reservedOpP "+" >> return NAdd)
  <|> (reservedOpP "-" >> return NSub)

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
        name <- identifierP
        return (NVar name)

-- ======================================================
-- PARSER DE STREXP
-- ======================================================

pStrExp :: Parser StrExp
pStrExp =
      try pStringConst -- try no es necesario ya que stringP e identifierP una emp con " y el otro con letra
  <|> pStringVar

pStringConst :: Parser StrExp
pStringConst = do
  s <- stringP
  return (SConst s)

pStringVar :: Parser StrExp
pStringVar = do
  name <- identifierP
  return (SVar name)

-- ======================================================
-- PARSER DE BOOLEXP
-- ======================================================
pBoolExp :: Parser BoolExp
pBoolExp = parseOr

-- nivel más alto: ||
parseOr :: Parser BoolExp
parseOr = chainl1 parseAnd orOp

orOp :: Parser (BoolExp -> BoolExp -> BoolExp)
orOp =
  reservedOpP "||" >> return Or

-- nivel medio: &&
parseAnd :: Parser BoolExp
parseAnd = chainl1 parseNot andOp

andOp :: Parser (BoolExp -> BoolExp -> BoolExp)
andOp =
  reservedOpP "&&" >> return And

-- nivel siguiente: !
parseNot :: Parser BoolExp
parseNot =
      (reservedOpP "!" >> do
          b <- parseNot
          return (Not b))
  <|> pBoolTerm

-- nivel base:
-- comparaciones, exists, literals, variables, paréntesis
pBoolTerm :: Parser BoolExp
pBoolTerm =
      parensP pBoolExp
  <|> try pExists
  <|> try pIsNull
  <|> try pEqBool
  <|> try pEqNum
  <|> try pEqStr
  <|> try pRelational
  <|> try pBoolLiteral
  <|> pBoolVariable

pBoolLiteral :: Parser BoolExp
pBoolLiteral =
      (reservedP "true" >> return BTrue)
  <|> (reservedP "false" >> return BFalse)

pBoolVariable :: Parser BoolExp
pBoolVariable = do
  name <- identifierP
  return (BVar name)

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

pEqBool :: Parser BoolExp
pEqBool = do
  e1 <- pBoolComparable
  op <- pEqBoolOp
  e2 <- pBoolComparable
  return (op e1 e2)

pEqBoolOp :: Parser (BoolExp -> BoolExp -> BoolExp)
pEqBoolOp =
      (reservedP "eqB" >> return EqBool)
  <|> (reservedP "neqB" >> return NeqBool)

-- esto evita recursión infinita:
-- NO usa pBoolExp completo
-- solo booleanos "comparables"
pBoolComparable :: Parser BoolExp
pBoolComparable =
      parensP pBoolExp
  <|> try pExists
  <|> try pIsNull
  <|> try pRelational
  <|> try pBoolLiteral
  <|> pBoolVariable

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

pPathExp :: Parser PathExp
pPathExp = do
  base <- identifierP
  fields <- many (reservedOpP "." >> identifierP)
  return (foldl PAccess (PVar base) fields)

-- ======================================================
-- PARSER DE JSONEXP
-- ======================================================
pJsonExp :: Parser JsonExp
pJsonExp =
      try pJObject
  <|> try pJArray
  <|> try pJNull
  <|> try pJBool
  <|> try pJNum
  <|> try pJStr
  <|> pJPath

pJNull :: Parser JsonExp
pJNull = do
  reservedP "null"
  return JNull

pJBool :: Parser JsonExp
pJBool =
      (reservedP "true" >> return (JBool BTrue))
  <|> (reservedP "false" >> return (JBool BFalse))

pJNum :: Parser JsonExp
pJNum = do
  n <- pNumExp
  return (JNum n)

pJStr :: Parser JsonExp
pJStr = do
  s <- pStrExp
  return (JStr s)

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

pJFieldNum :: Parser (FieldName, JsonExp)
pJFieldNum = do
  name <- identifierP
  reservedOpP ":n"
  value <- pNumExp
  return (name, JNum value)

pJFieldStr :: Parser (FieldName, JsonExp)
pJFieldStr = do
  name <- identifierP
  reservedOpP ":s"
  value <- pStrExp
  return (name, JStr value)

pJFieldBool :: Parser (FieldName, JsonExp)
pJFieldBool = do
  name <- identifierP
  reservedOpP ":b"
  value <- pBoolExp
  return (name, JBool value)

pJFieldNull :: Parser (FieldName, JsonExp)
pJFieldNull = do
  name <- identifierP
  reservedOpP ":nl"
  reservedP "null"
  return (name, JNull)

pJFieldPath :: Parser (FieldName, JsonExp)
pJFieldPath = do
  name <- identifierP
  reservedOpP ":p"
  value <- try pJObject <|> pJPath
  return (name, value)


pJArray :: Parser JsonExp
pJArray = do
  elems <- bracketsP (pJsonExp `sepBy` commaP)
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
  reservedOpP "."
  reservedP "filter"
  b <- parensP pBoolExp
  return (QFilter b)

-- Select

pSelect :: Parser QueryOp
pSelect = do
  reservedOpP "."
  reservedP "select"
  ids <- parensP (identifierP `sepBy1` commaP)
  return (QSelect ids)

-- Sort

pSort :: Parser QueryOp
pSort = do
  reservedOpP "."
  reservedP "sort"
  fields <- parensP (bracesP (pSortField `sepBy1` commaP))
  return (QSort fields)

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

-- Limit
pLimit :: Parser QueryOp
pLimit = do
  reservedOpP "."
  reservedP "limit"
  n <- parensP integerP
  return (QLimit (fromInteger n))

-- GroupBy + Aggregaciones + Having
pGroup :: Parser QueryOp
pGroup = do
  reservedOpP "."
  reservedP "groupby"
  fields <- parensP (identifierP `sepBy1` commaP)
  aggs <- many (try pAggregate)
  hav <- optionMaybe (try pHaving)
  return (QGroup (GroupSpec fields aggs hav))


pAggregate :: Parser Aggregate
pAggregate =
      try pCount
  <|> try pSum
  <|> try pAvg
  <|> try pMin
  <|> try pMax


pCount :: Parser Aggregate
pCount = do
  reservedOpP "."
  reservedP "count"
  (alias, field) <- parensP ( do
    a <- stringP
    commaP
    b <- identifierP
    return (a, b))
  return (Aggregate AggCount field alias)


pSum :: Parser Aggregate
pSum = do
  reservedOpP "."
  reservedP "sum"
  (alias, field) <- parensP ( do
    a <- stringP
    commaP
    b <- identifierP
    return (a, b))
  return (Aggregate AggSum field alias)


pAvg :: Parser Aggregate
pAvg = do
  reservedOpP "."
  reservedP "avg"
  (alias, field) <- parensP ( do
    a <- stringP
    commaP
    b <- identifierP
    return (a, b))
  return (Aggregate AggAvg field alias)


pMin :: Parser Aggregate
pMin = do
  reservedOpP "."
  reservedP "min"
  (alias, field) <- parensP ( do
    a <- stringP
    commaP
    b <- identifierP
    return (a, b))
  return (Aggregate AggMin field alias)


pMax :: Parser Aggregate
pMax = do
  reservedOpP "."
  reservedP "max"
  (alias, field) <- parensP ( do
    a <- stringP
    commaP
    b <- identifierP
    return (a, b))
  return (Aggregate AggMax field alias)


pHaving :: Parser BoolExp
pHaving = do
  reservedOpP "."
  reservedP "having"
  b <- parensP pBoolExp
  return b

-- Terminales

pTerminal :: Parser QueryTerminal
pTerminal =
      try pPreview
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
  path <- parensP parseJsonPath
  return (TerminalSave path)

parseJsonPath :: Parser JsonPath
parseJsonPath = do
  char '"'
  name <- many1 (alphaNum <|> char '_' <|> char '-')
  string ".json"
  char '"'
  return (name ++ ".json")


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


pCreateCollection :: Parser Comm
pCreateCollection = do
  reservedP "createCollection"
  reservedOpP "."
  col <- identifierP
  parensP (return ())
  return (CommCreateColl col)

pDropCollection :: Parser Comm
pDropCollection = do
  reservedP "dropCollection"
  reservedOpP "."
  col <- identifierP
  parensP (return ())
  return (CommDropColl col)

pInsert :: Parser Comm
pInsert = do
  reservedP "insert"
  reservedOpP "."
  col <- identifierP
  doc <- parensP pJsonExp
  return (CommInsert col doc)

pInsertManyComm :: Parser Comm
pInsertManyComm = do
  reservedP "insertMany"
  reservedOpP "."
  col <- identifierP
  docs <- parensP (bracketsP (pJsonExp `sepBy1` commaP))
  return (CommInsertMany col docs)

pUpdateOneComm :: Parser Comm
pUpdateOneComm = do
  reservedP "updateOne"
  reservedOpP "."
  col <- identifierP
  (cond, doc) <- parensP ( do
    c <- pBoolExp
    commaP
    d <- pJsonExp
    return (c, d))
  return (CommUpdateOne col cond doc)

pUpdateManyComm :: Parser Comm
pUpdateManyComm = do
  reservedP "updateMany"
  reservedOpP "."
  col <- identifierP
  (cond, doc) <- parensP ( do
    c <- pBoolExp
    commaP
    d <- pJsonExp
    return (c, d))
  return (CommUpdateMany col cond doc)

pDeleteComm :: Parser Comm
pDeleteComm = do
  reservedP "delete"
  reservedOpP "."
  col <- identifierP
  cond <- parensP pBoolExp
  return (CommDelete col cond)

pTransactionComm :: Parser Comm
pTransactionComm = do
  reservedP "transaction"
  commList <- bracesP (pSingleStatement `sepBy1` semiP)
  return (CommTransaction commList)

pTimestampComm :: Parser Comm
pTimestampComm = do
  reservedP "timestamp"
  reservedOpP "."
  target <- pTimestampTarget
  label <- parensP stringP
  return (CommTimestamp target label)

pRollbackComm :: Parser Comm
pRollbackComm = do
  reservedP "rollback"
  reservedOpP "."
  target <- pTimestampTarget
  label <- parensP stringP
  return (CommRollback target label)

pTimestampTarget :: Parser TimestampTarget
pTimestampTarget =
      do
        reservedP "database"
        return TSDatabase
  <|> do
        name <- identifierP
        return (TSColl name)

pCreateViewComm :: Parser Comm
pCreateViewComm = do
  reservedP "createView"
  (name, findQ) <- parensP ( do
    n <- stringP
    commaP
    f <- pFind
    return (n, f))
  return (CommCreateView name findQ)

pUseViewComm :: Parser Comm
pUseViewComm = do
  reservedP "useView"
  viewName <- parensP stringP
  option <- pViewOption viewName
  return (CommUseView viewName option)

-- parser de ViewOption
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
      try pSkip
  <|> try pTransactionComm
  <|> try pCreateCollection
  <|> try pDropCollection
  <|> try pCreateViewComm
  <|> try pUseViewComm
  <|> try pTimestampComm
  <|> try pRollbackComm
  <|> try pInsertManyComm
  <|> try pUpdateOneComm
  <|> try pUpdateManyComm
  <|> try pDeleteComm
  <|> try pInsert
  <|> do
        q <- pFind
        return (CommQuery q)

-- Programa completo

pProgram :: Parser Comm
pProgram = totParser pStatement
