module Parser where

import Control.Monad (guard)
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
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
      [ "find", "true", "false", "null"
      , "asc", "desc"
      , "insert", "insertMany", "updateOne", "updateMany", "delete"
      , "groupby", "having"
      , "count", "sum", "avg", "min", "max"
      , "preview", "save"
      , "transaction", "timestamp", "rollback"
      , "createView", "useView"
      , "database"
      , "exists", "skip"
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
semiP       = semi dsl
whiteSpaceP = whiteSpace dsl

bracketsP :: Parser a -> Parser a
bracketsP = brackets dsl

pExists :: Parser BoolExp
pExists = do
  reservedP "exists"
  e <- parensP pExp
  return (Exists e)


pExp :: Parser Exp
pExp = parseAddSub

parseAddSub :: Parser Exp
parseAddSub = chainl1 parseMulDiv addSubOp

addSubOp :: Parser (Exp -> Exp -> Exp)
addSubOp =
      (reservedOpP "+" >> return AddExp)
  <|> (reservedOpP "-" >> return SubExp)

parseMulDiv :: Parser Exp
parseMulDiv = chainl1 parseFactor mulDivOp

mulDivOp :: Parser (Exp -> Exp -> Exp)
mulDivOp =
      (reservedOpP "*" >> return MulExp)
  <|> (reservedOpP "/" >> return DivExp)

parseFactor :: Parser Exp
parseFactor =
      parensP pExp
  <|> try parseFieldAccess
  <|> try (do
        f <- floatP
        return (FloatExp f))
  <|> do
        i <- integerP
        return (IntExp (fromInteger i))
  <|> do
        s <- stringP
        return (StringExp s)
  <|> do
        reservedP "true"
        return (BoolExpVal True)
  <|> do
        reservedP "false"
        return (BoolExpVal False)
  <|> do
        reservedP "null"
        return NullExp
  <|> pObject
  <|> pArray

parseFieldAccess :: Parser Exp
parseFieldAccess = do
  name <- identifierP
  let base = VarExp name
  fields <- many (do
    reservedOpP "."
    identifierP
    )
  return (foldl FieldAccess base fields)

-- Objetos y Arrays JSON     
pObject :: Parser Exp
pObject = do
  fields <- bracesP (pField `sepBy` commaP)
  return (JObjectExp fields)

pField :: Parser (FieldName, Exp)
pField = do
  f <- identifierP
  reservedOpP ":"
  v <- pExp
  return (f, v)

pArray :: Parser Exp
pArray = do
  elems <- bracketsP (pExp `sepBy` commaP)
  return (JArrayExp elems)

pBoolExp :: Parser BoolExp
pBoolExp = parseOr

parseOr :: Parser BoolExp
parseOr = chainl1 parseAnd orOp

orOp :: Parser (BoolExp -> BoolExp -> BoolExp)
orOp = reservedOpP "||" >> return Or

parseAnd :: Parser BoolExp
parseAnd = chainl1 parseNot andOp

andOp :: Parser (BoolExp -> BoolExp -> BoolExp)
andOp = reservedOpP "&&" >> return And

parseNot :: Parser BoolExp
parseNot =
      (reservedOpP "!" >> do
          b <- parseNot
          return (Not b))
  <|> pBoolTerm

pBoolTerm :: Parser BoolExp
pBoolTerm =
      parensP pBoolExp
  <|> try pExists
  <|> pComparison
  <|> do
        reservedP "true"
        return BTrue
  <|> do
        reservedP "false"
        return BFalse

pComparison :: Parser BoolExp
pComparison = do
  e1 <- pExp
  op <- compOp
  e2 <- pExp
  return (op e1 e2)

compOp :: Parser (Exp -> Exp -> BoolExp)
compOp =
      do
        reservedOpP "=="
        return Eq
  <|> do
        reservedOpP "!="
        return Neq
  <|> do
        reservedOpP ">"
        return Gt
  <|> do
        reservedOpP ">="
        return Ge
  <|> do
        reservedOpP "<"
        return Lt
  <|> do
        reservedOpP "<="
        return Le

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
  reservedP "Skip"
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
  doc <- parensP pExp
  return (CommInsert col doc)

pInsertManyComm :: Parser Comm
pInsertManyComm = do
  reservedP "insertMany"
  reservedOpP "."
  col <- identifierP
  listDoc <- parensP (bracketsP (pExp `sepBy1` commaP))
  return (CommInsertMany col listDoc)

pUpdateOneComm :: Parser Comm
pUpdateOneComm = do  
  reservedP "updateOne"
  reservedOpP "."
  col <- identifierP
  (cond, doc) <- parensP ( do
    c <- pBoolExp
    commaP
    d <- pExp
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
    d <- pExp
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
  --reservedOpP "."
  commList <- bracesP (pStatement `sepBy1` semiP)
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

