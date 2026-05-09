
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) =
    Reader (\r -> f (g r))

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x =
    Reader (\_ -> x)

  liftA2 :: (a -> b -> c)-> Reader r a-> Reader r b-> Reader r c
  liftA2 f (Reader ra) (Reader rb) =
    Reader (\r -> f (ra r) (rb r))

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= f =
    Reader (\r ->
      let a = ra r
          Reader rb = f a
      in rb r
    )

ask :: Reader r r
ask = Reader id


asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) =
  Reader (\r -> ra (f r))


data BankConfig = BankConfig
  { interestRate   :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Show)

data Account = Account
  { accountId :: String
  , balance   :: Int
  } deriving (Show)


calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  pure $ round (fromIntegral (balance acc) * rate)

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  pure acc
    { balance = balance acc - fee
    }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBal <- asks minimumBalance
  pure (balance acc >= minBal)


processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  updatedAcc <- applyTransactionFee acc
  interest   <- calculateInterest acc
  valid      <- checkMinimumBalance acc
  pure (updatedAcc, interest, valid)

-- ghci> let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
-- ghci> let acc = Account { accountId = "A-001", balance = 1000 }
-- ghci> runReader (processAccount acc) cfg
-- (Account {accountId = "A-001", balance = 998}, 50, True)