import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad (guard)
import Data.List (permutations)
import Control.Monad.Writer (Writer, tell)

--Task 1
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  neighbours <- M.lookup pos maze
  M.lookup dir neighbours

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ pos []     = Just pos
followPath maze pos (d:ds) = do
  next <- move maze pos d
  followPath maze next ds

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = go start dirs
  where
    go pos []     = Just [pos]
    go pos (d:ds) = do
      next <- move maze pos d
      rest <- go next ds
      return (pos : rest)

maze :: Maze
maze = M.fromList
  [ ((0,0), M.fromList [(E,(1,0)), (S,(0,1))])
  , ((1,0), M.fromList [(W,(0,0))])
  , ((0,1), M.fromList [(N,(0,0))])
  ]
--Task 2
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`M.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)
--Task 3
type Guest = String
type Conflict = (Guest, Guest)

conflicts :: [Conflict] -> Guest -> Guest -> Bool
conflicts cs a b = (a,b) `elem` cs || (b,a) `elem` cs

valid :: [Conflict] -> [Guest] -> Bool
valid _ [] = True
valid cs gs = all ok pairs
  where
    pairs = zip gs (tail gs ++ [head gs])
    ok (a,b) = not (conflicts cs a b)
seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings [] _ = []
seatings (g:gs) cs = do
  perm <- permutations gs
  let seating = g : perm
  guard (valid cs seating)
  return seating

--Task 4
data Result a = Failure String | Success a [String]
  deriving (Show)

instance Functor Result where
  fmap _ (Failure msg) = Failure msg
  fmap f (Success x ws) = Success (f x) ws

instance Applicative Result where
  pure x = Success x []

  Failure msg <*> _ = Failure msg
  Success f w1 <*> r = case r of
    Failure msg     -> Failure msg
    Success x w2    -> Success (f x) (w1 ++ w2)

instance Monad Result where
  Failure msg >>= _ = Failure msg
  Success x w1 >>= f = case f x of
    Failure msg     -> Failure msg
    Success y w2    -> Success y (w1 ++ w2)

warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

validateAge :: Int -> Result Int
validateAge n
  | n < 0     = failure "Negative age"
  | n > 150   = do
      warn "Age unusually high"
      return n
  | otherwise = return n

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

--Task 5
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Show)

simplify :: Expr -> Writer [String] Expr
simplify expr = case expr of
  Lit n -> return (Lit n)

  Neg e -> do
    e' <- simplify e
    case e' of
      Neg x -> do
        tell ["Double negation: --e -> e"]
        return x
      _ -> return (Neg e')

  Add e1 e2 -> do
    e1' <- simplify e1
    e2' <- simplify e2
    case (e1', e2') of
      (Lit 0, e) -> tell ["Add identity: 0 + e -> e"] >> return e
      (e, Lit 0) -> tell ["Add identity: e + 0 -> e"] >> return e
      (Lit a, Lit b) -> do
        tell ["Constant fold: a + b"]
        return (Lit (a + b))
      _ -> return (Add e1' e2')

  Mul e1 e2 -> do
    e1' <- simplify e1
    e2' <- simplify e2
    case (e1', e2') of
      (Lit 0, _) -> tell ["Zero absorption: 0 * e -> 0"] >> return (Lit 0)
      (_, Lit 0) -> tell ["Zero absorption: e * 0 -> 0"] >> return (Lit 0)
      (Lit 1, e) -> tell ["Mul identity: 1 * e -> e"] >> return e
      (e, Lit 1) -> tell ["Mul identity: e * 1 -> e"] >> return e
      (Lit a, Lit b) -> do
        tell ["Constant fold: a * b"]
        return (Lit (a * b))
      _ -> return (Mul e1' e2')

--Task 6
newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)
instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)


-- pure id <*> ZipList [1,2,3]
-- ZipList [1,2,3]

-- pure (+) <*> ZipList [1,2,3] <*> ZipList [10,20,30]
-- ZipList [11,22,33]


-- ZipList cannot have a lawful Monad instance because >>= requires combining results from f :: a -> ZipList b into a single structure, but those results may have different lengths. Since ZipList combines elements position-wise, there is no consistent way to align or flatten these varying-length lists. This mismatch breaks the requirements of a monad.
