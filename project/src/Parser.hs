module Parser where

import AST
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String


sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol' sc 

punc :: String -> Parser String
punc = L.symbol sc

parens :: Parser a -> Parser a
parens = between (punc "(") (punc ")")

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

parseValue :: Parser Value
parseValue = choice
  [ IntV <$> lexeme L.decimal
  , StrV <$> lexeme (char '"' >> manyTill L.charLiteral (char '"'))
  , BoolV True  <$ symbol "TRUE"
  , BoolV False <$ symbol "FALSE"
  ]

parseOp :: Parser CmpOp
parseOp = choice
  [ Eq  <$ punc "="
  , NEq <$ punc "!="
  , Le  <$ punc "<="
  , Ge  <$ punc ">="
  , Lt  <$ punc "<"
  , Gt  <$ punc ">"
  ]


parseAtomCond :: Parser Cond
parseAtomCond = do
  leftCol <- identifier
  op <- parseOp
  (CmpCol leftCol op <$> parseValue) <|> (CmpCols leftCol op <$> identifier)

parseCond :: Parser Cond
parseCond = do
  left <- parseAtomCond
  rest left
  where
    rest left = 
      (symbol "AND" >> parseAtomCond >>= \right -> rest (And left right))
      <|> (symbol "OR"  >> parseAtomCond >>= \right -> rest (Or  left right))
      <|> return left


parseCreateTable :: Parser Statement
parseCreateTable = do
  symbol "CREATE" >> symbol "TABLE"
  tableName <- identifier
  let colTypeP = (TInt <$ symbol "int") <|> (TString <$ symbol "string") <|> (TBool <$ symbol "bool")
  let colDefP  = ColumnDef <$> identifier <*> colTypeP
  defs <- parens (colDefP `sepBy` punc ",")
  return $ CreateTable tableName defs

parseInsert :: Parser Statement
parseInsert = do
  symbol "INSERT" >> symbol "INTO"
  tableName <- identifier
  symbol "VALUES"
  vals <- parens (parseValue `sepBy` punc ",")
  return $ Insert tableName vals

parseSelect :: Parser Statement
parseSelect = do
  symbol "SELECT"
 
  cols <- (symbol "*" >> return ["*"]) <|> (identifier `sepBy` punc ",")
  symbol "FROM"
  table1 <- identifier
  
  mJoin <- optional $ do
    symbol "JOIN"
    table2 <- identifier
    symbol "ON"
    cond <- parseCond
    return (table2, cond)

  mWhere <- optional $ do
    symbol "WHERE"
    parseCond
    

  let baseQuery = case mJoin of
        Nothing -> From table1
        Just (t2, joinCond) -> Join (From table1) (From t2) joinCond

  let filteredQuery = case mWhere of
        Nothing -> baseQuery
        Just whereCond -> Filter whereCond baseQuery

  return $ SelectStmt (Project cols filteredQuery)


parseSQL :: Parser Statement
parseSQL = do
  sc
  stmt <- parseCreateTable <|> parseInsert <|> parseSelect
  optional (punc ";")
  eof
  return stmt