module AST where

import Data.Map (Map)


data Value 
  = IntV Int 
  | StrV String 
  | BoolV Bool 
  | NullV 
  deriving (Show, Eq, Ord)

type Row = Map String Value

type Table = [Row]

type Database = Map String Table

data Query
  = From String               
  | Project [String] Query    
  | Filter Cond Query          
  | Join Query Query Cond       
  deriving (Show, Eq)

data Cond
  = CmpCol String CmpOp Value   
  | CmpCols String CmpOp String
  | And Cond Cond
  | Or Cond Cond
  deriving (Show, Eq)

data CmpOp = Eq | NEq | Lt | Gt | Le | Ge deriving (Show, Eq)

data ColType = TInt | TString | TBool deriving (Show, Eq)

data ColumnDef = ColumnDef { colName :: String, colType :: ColType } deriving (Show, Eq)

data Statement
  = CreateTable String [ColumnDef]
  | Insert String [Value]
  | SelectStmt Query    
  deriving (Show, Eq)