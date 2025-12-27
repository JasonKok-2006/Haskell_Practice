{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use splitAt" #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}
{-# HLINT ignore "Use isNothing" #-}


{-

These are solutions to the 99 haskell problems

-}

-- Problem 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [x] = Nothing
myButLast [x,y] = Just x
myButLast (x:xs) = myButLast xs

-- Problem 3 (What a shit question, indexing starts at 1 not 0.)
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) n | n == 1 = Just x
                   | n <= 0 = Nothing
                   | otherwise = elementAt xs (n-1)

-- Problem 4
myLength :: [a] -> Int
myLength = foldr (\x -> (+) 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse xs = myReverseHelper xs []

myReverseHelper :: [a] -> [a] -> [a]
myReverseHelper [] ys = ys
myReverseHelper (x:xs) ys = myReverseHelper xs (x:ys)

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List x) = concatMap flatten x

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress rest
            where
                (equal, rest) = span (==x) xs

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:equal) : pack rest
            where
                (equal, rest) = span (==x) xs

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\y -> (length y, head y)) ys
            where
                ys = pack xs

-- Problem 11
data Encoding a = Single a | Multiple Int a
    deriving Show

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified xs = map (\(n, x) -> if n == 1 then Single x else Multiple n x) (encode xs)

-- Problem 12
decodeModified :: [Encoding a] -> [a]
decodeModified = concatMap decoder

decoder :: Encoding a -> [a]
decoder (Single a) = [a]
decoder (Multiple n a) = buildList (n-1) a

buildList :: Int -> a -> [a]
buildList 0 x = [x]
buildList n x = x : buildList (n-1) x

-- Problem 13
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect xs = map (\(n, x) -> Multiple n x) (encode xs)

-- Problem 14
dupli :: [a] -> [a]
dupli xs = concatMap (\x -> [x, x]) xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (buildList (n-1)) xs

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) n = (x : fst (split xs (n-1)), snd (split xs (n-1)))

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs n m = snd $ split (fst $ split xs m) (n-1)

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n | n >= 0 = snd (split xs n) ++ fst (split xs n)
            | otherwise = snd (split xs (length xs + n)) ++ fst (split xs (length xs + n))

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Index out of bounds"
removeAt n (x:xs) | n == 0 = (x, xs)
                  | otherwise = (fst (removeAt (n-1) xs), x : snd (removeAt (n-1) xs))

-- Problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt y (x:xs) n | n == 0 = y : x : xs
                    | otherwise = x : insertAt y xs (n-1)

-- Problem 22
range :: Int -> Int -> [Int]
range x y = [x..y]

{-

Problems 23, 24 and 25 use the RandomGen class. This is removed in modern haskell, making these
problems useless.

-}

-- Problem 26
combinations :: Ord a => Int -> [a] -> [[a]]
combinations n xs = map (reverse . fst) (combinationsHelper n [([], xs)])

combinationsHelper :: Ord a => Int -> [([a], [a])] -> [([a], [a])]
combinationsHelper 0 xs = xs
combinationsHelper n xs = combinationsHelper (n-1) (concatMap removeAtAllCaller xs)

removeAtAllCaller :: Ord a => ([a], [a]) -> [([a], [a])]
removeAtAllCaller xs = removeAtAll 0 xs

removeAtAll :: Ord a => Int -> ([a], [a]) -> [([a], [a])]
removeAtAll n (xs, ys) | n == length ys = []
                       | otherwise = if maybeHead xs == Nothing || item > head xs
                                        then (item : xs, residual) : removeAtAll (n+1) (xs, ys)
                                        else removeAtAll (n+1) (xs, ys)
                    where
                        (item, residual) = removeAt n ys

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

-- Problem 27 (Some maths shit I don't understand.)

-- Problem 28
lsort :: [[a]] -> [[a]]
lsort xs = lsortHelper xs []

lsortHelper :: [[a]] -> [[a]] -> [[a]]
lsortHelper [] ys = ys
lsortHelper (x:xs) ys = lsortHelper xs (insertSubList x ys)

insertSubList :: [a] -> [[a]] -> [[a]]
insertSubList x [] = [x]
insertSubList x (y:ys) | length x > length y = y : insertSubList x ys
                       | otherwise = x : y : ys

