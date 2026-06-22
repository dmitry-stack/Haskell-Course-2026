module Main where

import AST
import Engine
import Parser
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Text.Megaparsec (parse, errorBundlePretty)
import Optimizer
import Tests
import UnitTests


makeRow :: [(String, Value)] -> Row
makeRow = Map.fromList

usersTable :: Table
usersTable = 
  [ makeRow [("id", IntV 1), ("name", StrV "Alice"), ("age", IntV 25)]
  , makeRow [("id", IntV 2), ("name", StrV "Bob"),   ("age", IntV 30)]
  ]

ordersTable :: Table
ordersTable = 
  [ makeRow [("order_id", IntV 101), ("user_id", IntV 1), ("product", StrV "Laptop")]
  ]


initialDb :: Database
initialDb = Map.fromList 
  [ ("Users", usersTable)
  , ("Orders", ordersTable)
  ]

initialSchema :: Map String [String]
initialSchema = Map.fromList
  [ ("Users", ["id", "name", "age"])
  , ("Orders", ["order_id", "user_id", "product"])
  ]


main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn " SqlLiteClone: Interactive Haskell Engine "
  putStrLn " :tests  - run all tests                  "
  putStrLn " :demo   - show optimizer plan             "
  putStrLn " quit    - exit                            "
  putStrLn "==========================================\n"
  repl initialDb initialSchema



repl :: Database -> Map String [String] -> IO ()
repl db schema = do
  putStr "SQL> "
  hFlush stdout  
  input <- getLine
  
  if input `elem` ["quit", "exit", ":q"]
    then putStrLn "Goodbye!"
  else if input == ":tests"
    then do
      runUnitTests
      runTests
      repl db schema
  else if input == ":demo"
    then do
      putStrLn "\n[OPTIMIZER DEMO]"
      putStrLn $ "Before: " ++ show terribleQuery
      putStrLn $ "After:  " ++ show (optimize terribleQuery)
      repl db schema
  else case parse parseSQL "" input of
      Left err -> do
        putStrLn $ errorBundlePretty err
        repl db schema
      Right stmt -> executeStmt db schema stmt
executeStmt :: Database -> Map String [String] -> Statement -> IO ()
executeStmt db schema stmt = case stmt of

  SelectStmt query -> do
    let optimizedPlan = optimize query
    putStrLn "\n[EXPLAIN PLAN]"
    if optimizedPlan == query
      then putStrLn $ "Plan: " ++ show query
      else do
        putStrLn $ "Original : " ++ show query
        putStrLn $ "Optimized: " ++ show optimizedPlan
    putStrLn "--------------"
    case evalQuery db optimizedPlan of
      Left err -> putStrLn err
      Right table -> do
        mapM_ print table
        putStrLn $ "(" ++ show (length table) ++ " rows returned)"
    repl db schema

  CreateTable tableName colDefs -> do
    let colNames = map colName colDefs
    let newSchema = Map.insert tableName colNames schema
    let newDb = Map.insert tableName [] db
    putStrLn $ "Table '" ++ tableName ++ "' created successfully."
    repl newDb newSchema

  Insert tableName vals -> do
    case Map.lookup tableName schema of
      Nothing -> do
        putStrLn $ "Error: Table '" ++ tableName ++ "' does not exist."
        repl db schema
      Just colNames -> do
        if length colNames /= length vals
          then do
            putStrLn "Error: Inserted values do not match column count."
            repl db schema
          else do
            let newRow = Map.fromList (zip colNames vals)
            let oldTable = Map.findWithDefault [] tableName db
            let newDb = Map.insert tableName (oldTable ++ [newRow]) db
            putStrLn "1 row inserted."
            repl newDb schema


terribleQuery :: Query
terribleQuery =
  Filter (CmpCol "age" Gt (IntV 18))
    (Join
      (Filter (CmpCol "name" Eq (StrV "Alice")) (From "Users"))
      (From "Orders")
      (CmpCols "id" Eq "user_id"))