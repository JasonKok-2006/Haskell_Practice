
data BT a = Empty | Fork a (BT a) (BT a) deriving Show

memoBT :: (Integer -> a) -> (Integer -> a)
memoBT = tfetch . tstore

fixBT :: ((Integer -> a) -> (Integer -> a)) -> Integer -> a
fixBT f = g
 where
   g = f (memoBT g)

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fibstep :: (Integer -> Integer) -> (Integer -> Integer)
fibstep g = h
  where
    h n
      | n <= 1    = 1
      | otherwise = g (n-2) + g (n-1)

same :: Integer -> Integer
same n = n

fibBT :: Integer -> Integer
fibBT = fixBT fibstep

tstore :: (Integer -> a) -> BT a
tstore f = build f 0

tfetch :: BT a -> (Integer -> a)
tfetch t k = traverseTree t (route k)

traverseTree :: BT a -> [Integer] -> a
traverseTree (Fork v _ _) [] = v
traverseTree (Fork _ l r) (x:xs)
  | x == 1    = traverseTree l xs
  | otherwise = traverseTree r xs

route :: Integer -> [Integer]
route n = reverse (route' n)

route' :: Integer -> [Integer]
route' n
  | n == 0    = []
  | odd n     = 1 : route' ((n - 1) `div` 2)
  | otherwise = 2 : route' ((n - 2) `div` 2)


build :: (Integer -> a) -> Integer -> BT a
build f n = Fork (f n) (build f ((2*n)+1)) (build f ((2*n)+2))


equalFunctions :: (Eq a) => Integer -> (Integer -> a) -> (Integer -> a) -> Bool
equalFunctions n f1 f2 = and [f1 i == f2 i | i <- [0..n]]