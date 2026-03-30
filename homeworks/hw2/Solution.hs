
module Homework where

import Data.Monoid

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
    deriving (Show)

seq1 :: Sequence Int
seq1 = Append (Single 1) (Append (Single 2) (Single 3))


instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)



instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = foldr (:) []

seqLength :: Sequence a -> Int
seqLength = foldr (\_ acc -> acc + 1) 0


instance Semigroup (Sequence a) where
    (<>) = Append

instance Monoid (Sequence a) where
    mempty = Empty


tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x seq = go [seq]
  where
    go [] = False
    go (Empty : rest) = go rest
    go (Single y : rest)
        | x == y    = True
        | otherwise = go rest
    go (Append l r : rest) = go (l : r : rest)


tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
  where
    go [] acc = reverse acc
    go (Empty : rest) acc = go rest acc
    go (Single x : rest) acc = go rest (x : acc)
    go (Append l r : rest) acc = go (l : r : rest) acc


data Token = TNum Int | TAdd | TSub | TMul | TDiv
    deriving (Show)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing

    go (t:ts) stack =
        case t of
            TNum n -> go ts (n : stack)

            TAdd -> binOp (+) ts stack
            TSub -> binOp (-) ts stack
            TMul -> binOp (*) ts stack

            TDiv -> case stack of
                (x:y:rest) ->
                    if x == 0 then Nothing
                    else go ts ((y `div` x) : rest)
                _ -> Nothing

    binOp op ts (x:y:rest) = go ts (op y x : rest)
    binOp _ _ _ = Nothing

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0


encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x,1)]
    step x ((y,n):ys)
        | x == y    = (y, n+1) : ys
        | otherwise = (x,1) : (y,n) : ys



decode :: [(a, Int)] -> [a]
decode = foldr (\(x,n) acc -> replicate n x ++ acc) []


main :: IO()
main = do
  print "fmap"
  print (fmap (+1) seq1)
  print "seqToList"
  print(seqToList seq1)
  print "seqLength"
  print(seqLength seq1)
  print "Semigroup"
  print(Single 2 <> Single 3)
  print "tailElem"
  print(tailElem 2 seq1)
  print(tailElem 9 seq1)
  print "tailToList"
  print(tailToList seq1)
  print "tailRPN"
  print(tailRPN[TNum 3, TNum 2, TAdd])
  print "myReverse"
  print(myReverse[1,2,3])
  print "myTakeWhile"
  print(myTakeWhile even [2,3,6])
  print "decimal"
  print(decimal[1,2,3])
  print "encode"
  print(encode "aaabbbccc")
  print "decode"
  print(decode [('a',3),('b',4)])



