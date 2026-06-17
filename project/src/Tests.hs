module Tests where

import Test.QuickCheck
import AST
import Engine
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersect)


instance Arbitrary Value where
  arbitrary = oneof 
    [ IntV <$> arbitrary
    , StrV <$> elements ["Alice", "Bob", "Charlie", "Data"]
    , BoolV <$> arbitrary
    ]

genRow :: Gen Row
genRow = do
  idVal <- arbitrary :: Gen Int
  nameVal <- elements ["Alice", "Bob", "Charlie"]
  ageVal <- arbitrary :: Gen Int
  return $ Map.fromList [("id", IntV idVal), ("name", StrV nameVal), ("age", IntV ageVal)]


genTable :: Gen Table
genTable = listOf genRow

getSuccess :: Either String Table -> Table
getSuccess (Right t) = t
getSuccess (Left e)  = error e

prop_select_true :: Property
prop_select_true = forAll genTable $ \table ->
  let 
    db = Map.singleton "TestTable" table
    query = Filter (CmpCol "id" Eq (IntV 1)) (From "TestTable") 

    alwaysTrueCond = CmpCols "id" Eq "id"
    trueQuery = Filter alwaysTrueCond (From "TestTable")
  in
    getSuccess (evalQuery db trueQuery) == table


prop_join_false :: Property
prop_join_false = forAll genTable $ \table ->
  let 
    db = Map.fromList [("T1", table), ("T2", table)]
  
    alwaysFalseCond = CmpCols "id" NEq "id"
    joinQuery = Join (From "T1") (From "T2") alwaysFalseCond
  in
    null $ getSuccess (evalQuery db joinQuery)


prop_project_intersect :: [String] -> [String] -> Property
prop_project_intersect xs ys = forAll genTable $ \table ->
  let
    db = Map.singleton "TestTable" table
    

    leftQuery = Project xs (Project ys (From "TestTable"))
    leftResult = getSuccess (evalQuery db leftQuery)
    
    rightQuery = Project (xs `intersect` ys) (From "TestTable")
    rightResult = getSuccess (evalQuery db rightQuery)
  in
    leftResult == rightResult

runTests :: IO ()
runTests = do
  putStrLn "Checking Property 1: Select True == Full Table"
  quickCheck prop_select_true
  
  putStrLn "\nChecking Property 2: Join False == Empty Table"
  quickCheck prop_join_false
  
  putStrLn "\nChecking Property 3: Project xs . Project ys == Project (xs `intersect` ys)"
  quickCheck prop_project_intersect