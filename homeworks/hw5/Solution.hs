module Homework05 where

import Control.Monad.State
import Control.Monad (when)
import qualified Data.Map as Map
import Data.Map (Map)

-- Task1
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show, Eq)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) =
  modify (n :)

execInstr POP = do
  st <- get
  case st of
    (_:xs) -> put xs
    [] -> return ()

execInstr DUP = do
  st <- get
  case st of
    (x:xs) -> put (x:x:xs)
    [] -> return ()

execInstr SWAP = do
  st <- get
  case st of
    (x:y:xs) -> put (y:x:xs)
    _ -> return ()

execInstr ADD = do
  st <- get
  case st of
    (x:y:xs) -> put ((x + y) : xs)
    _ -> return ()

execInstr MUL = do
  st <- get
  case st of
    (x:y:xs) -> put ((x * y) : xs)
    _ -> return ()

execInstr NEG = do
  st <- get
  case st of
    (x:xs) -> put ((-x) : xs)
    []     -> return ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []


--Task2

data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq Expr Expr
  deriving (Show, Eq)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n

eval (Var name) = do
  env <- get
  return (env Map.! name)

eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)

eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)

eval (Neg e) = do
  v <- eval e
  return (-v)

eval (Assign name expr) = do
  value <- eval expr
  modify (Map.insert name value)
  return value

eval (Seq e1 e2) = do
  _ <- eval e1
  eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

-- Task3

editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int

editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just value ->
      return value

    Nothing -> do
      result <-
        case (i, j) of
          (0, _) -> return j
          (_, 0) -> return i
          _ ->
            if xs !! (i - 1) == ys !! (j - 1)
              then
                editDistM xs ys (i - 1) (j - 1)
              else do
                deletion     <- editDistM xs ys (i - 1) j
                insertion    <- editDistM xs ys i (j - 1)
                substitution <- editDistM xs ys (i - 1) (j - 1)

                return (1 + minimum [ deletion, insertion , substitution ])

      modify (Map.insert (i, j) result)
      return result

editDistance :: String -> String -> Int
editDistance xs ys = evalState (editDistM xs ys (length xs) (length ys)) Map.empty


--Treasure Hunters

data LocationType
  = Normal
  | DecisionPoint [String]
  | Obstacle Int
  | Treasure Int
  | Trap Int
  | Goal
  deriving (Show)

data GameState = GameState
  { playerPos   :: Int
  , playerScore :: Int
  , playerEnergy :: Int
  , board       :: Map Int LocationType
  } deriving (Show)

type AdventureGame a = StateT GameState IO a


movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  modify $ \gs ->
    gs
      { playerPos = playerPos gs + roll
      , playerEnergy = playerEnergy gs - roll
      }

  return roll

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  choice <- lift $ getPlayerChoice options
  return choice


handleLocation :: AdventureGame Bool
handleLocation = do
  gs <- get
  let pos = playerPos gs
      tile = Map.findWithDefault Normal pos (board gs)

  case tile of
    Normal -> do
      liftIO $ putStrLn "Nothing special here."
      return False

    DecisionPoint options -> do
      choice <- makeDecision options
      liftIO $
        putStrLn ("You chose: " ++ choice)
      return False

    Obstacle penalty -> do
      liftIO $
        putStrLn ("Obstacle! Lost energy: " ++ show penalty)

      modify $ \s ->
        s { playerEnergy = playerEnergy s - penalty }

      return False

    Treasure points -> do
      liftIO $
        putStrLn ("Treasure found! +" ++ show points ++ " points")

      modify $ \s ->
        s { playerScore = playerScore s + points }

      return False

    Trap penalty -> do
      liftIO $
        putStrLn ("Trap! Lost " ++ show penalty ++ " points")

      modify $ \s ->
        s { playerScore = max 0 (playerScore s - penalty) }

      return False

    Goal -> do
      liftIO $
        putStrLn "You found the main treasure!"
      return True



playTurn :: AdventureGame Bool
playTurn = do
  roll <- liftIO getDiceRoll

  moved <- movePlayer roll

  liftIO $
    putStrLn ("Moved " ++ show moved ++ " spaces.")

  finished <- handleLocation

  gs <- get

  when (playerEnergy gs <= 0) $
    liftIO $ putStrLn "Out of energy!"

  return (finished || playerEnergy gs <= 0)


playGame :: AdventureGame ()
playGame = do
  gs <- get
  liftIO $ displayGameState gs

  ended <- playTurn

  if ended
    then do
      finalState <- get
      liftIO $ do
        putStrLn "Game Over!"
        displayGameState finalState
    else
      playGame


getDiceRoll :: IO Int
getDiceRoll = do
  putStrLn "Enter dice roll (1-6):"
  input <- getLine

  case reads input of
    [(n, "")] | n >= 1 && n <= 6 ->
      return n

    _ -> do
      putStrLn "Invalid roll. Try again."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
  putStrLn ("Position: " ++ show (playerPos gs))
  putStrLn ("Score:    " ++ show (playerScore gs))
  putStrLn ("Energy:   " ++ show (playerEnergy gs))

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn "Choose an option:"

  mapM_
    (\(i, opt) ->
      putStrLn (show i ++ ". " ++ opt))
    (zip [1 :: Int ..] options)

  input <- getLine

  case reads input of
    [(n, "")] | n >= 1 && n <= length options ->
      return (options !! (n - 1))

    _ -> do
      putStrLn "Invalid choice."
      getPlayerChoice options


sampleBoard :: Map Int LocationType
sampleBoard =
  Map.fromList
    [ (3, Treasure 10)
    , (5, Obstacle 3)
    , (7, Trap 5)
    , (9, DecisionPoint ["Forest Path", "Mountain Path"])
    , (12, Treasure 20)
    , (15, Goal)
    ]

initialGameState :: GameState
initialGameState =
  GameState
    { playerPos = 0
    , playerScore = 0
    , playerEnergy = 20
    , board = sampleBoard
    }

-- evalStateT playGame initialGameState