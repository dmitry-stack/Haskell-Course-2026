module UnitTests where

import AST
import Engine
import Parser
import Text.Megaparsec (parse)
import Data.Map (Map)
import qualified Data.Map as Map

assertTrue :: String -> Bool -> IO ()
assertTrue name True  = putStrLn $ " PASS: " ++ name
assertTrue name False = putStrLn $ " FAIL: " ++ name



testParser :: IO ()
testParser = do
  putStrLn "\n--- Running Parser Unit Tests ---"
  
  let parseResult1 = parse parseSQL "" "SELECT name FROM Users WHERE age > 18;"
  let expectedAST1 = SelectStmt (Project ["name"] (Filter (CmpCol "age" Gt (IntV 18)) (From "Users")))
  assertTrue "Parses SELECT with WHERE" (parseResult1 == Right expectedAST1)

  let parseResult2 = parse parseSQL "" "INSERT INTO Pets VALUES (1, \"Dog\");"
  let expectedAST2 = Insert "Pets" [IntV 1, StrV "Dog"]
  assertTrue "Parses INSERT statement" (parseResult2 == Right expectedAST2)


mockRow1 :: Row
mockRow1 = Map.fromList [("A", IntV 1), ("B", IntV 10), ("Extra", IntV 99)]
mockRow2 :: Row
mockRow2 = Map.fromList [("A", IntV 2), ("B", IntV 20), ("Extra", IntV 99)]

mockDb :: Database
mockDb = Map.fromList [("T1", [mockRow1, mockRow2])]

testEngine :: IO ()
testEngine = do
  putStrLn "\n--- Running Engine Unit Tests ---"

  let projResult = evalQuery mockDb (Project ["A"] (From "T1"))
  let expectedProj = Right [Map.fromList [("A", IntV 1)], Map.fromList [("A", IntV 2)]]
  assertTrue "Project ignores extra columns" (projResult == expectedProj)

  let filterResult = evalQuery mockDb (Filter (CmpCol "B" Gt (IntV 15)) (From "T1"))
  let expectedFilter = Right [mockRow2] 
  assertTrue "Filter keeps only matching rows" (filterResult == expectedFilter)


  let joinDb = Map.fromList 
        [ ("LeftT",  [Map.fromList [("id", IntV 1), ("val", StrV "L")]])
        , ("RightT", [Map.fromList [("fk", IntV 1), ("val2", StrV "R")]])
        ]
  let joinResult = evalQuery joinDb (Join (From "LeftT") (From "RightT") (CmpCols "id" Eq "fk"))
  let expectedJoin = Right [Map.fromList [("id", IntV 1), ("val", StrV "L"), ("fk", IntV 1), ("val2", StrV "R")]]
  assertTrue "Join produces expected product-with-condition" (joinResult == expectedJoin)


runUnitTests :: IO ()
runUnitTests = do
  testParser
  testEngine
  putStrLn "\nDone."