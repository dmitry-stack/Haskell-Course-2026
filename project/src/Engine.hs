module Engine where

import AST
import Data.Map (Map)
import qualified Data.Map as Map


evalQuery :: Database -> Query -> Either String Table
evalQuery db (From tableName) =
  case Map.lookup tableName db of
    Just table -> Right table
    Nothing    -> Left $ "Error: Table '" ++ tableName ++ "' does not exist."

evalQuery db (Filter cond query) = do
  table <- evalQuery db query
  Right $ filter (evalCond cond) table

evalQuery db (Project columns query) = do
  table <- evalQuery db query
  if columns == ["*"]
    then Right table 
    else Right $ map (Map.filterWithKey (\colName _ -> colName `elem` columns)) table

evalQuery db (Join leftQuery rightQuery cond) = do
  leftTable  <- evalQuery db leftQuery
  rightTable <- evalQuery db rightQuery
  let crossJoin = [ Map.union leftRow rightRow | leftRow <- leftTable, rightRow <- rightTable ]
  Right $ filter (evalCond cond) crossJoin


evalCond :: Cond -> Row -> Bool
evalCond (CmpCol colName op targetVal) row =
  case Map.lookup colName row of
    Nothing -> False
    Just actualVal -> applyOp op actualVal targetVal

evalCond (CmpCols colA op colB) row =
  case (Map.lookup colA row, Map.lookup colB row) of
    (Just valA, Just valB) -> applyOp op valA valB
    _ -> False

evalCond (And c1 c2) row = evalCond c1 row && evalCond c2 row
evalCond (Or c1 c2) row  = evalCond c1 row || evalCond c2 row

applyOp :: CmpOp -> Value -> Value -> Bool
applyOp Eq  a b = a == b
applyOp NEq a b = a /= b
applyOp Lt  a b = a < b
applyOp Gt  a b = a > b
applyOp Le  a b = a <= b
applyOp Ge  a b = a >= b