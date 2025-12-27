{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use foldr" #-}
import Data.List (transpose)

pyths :: Int -> [(Int, Int, Int)]
pyths z = [(x,y,z) | x <- [1..z], y <- [1..z] , (x^2 + y^2) == z^2]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

perfect :: Int -> Bool
perfect n = sum (factors n) == n

factors :: Int -> [Int]
factors n = [m | m <- [1..n-1], n `mod` m == 0]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct [] [] = 0
scalarProduct (x:xs) (y:ys) = x*y + scalarProduct xs ys

matrixMul :: [[Int]] -> [[Int]] -> [[Int]]
matrixMul [] [] = []
matrixMul [] y = y
matrixMul x [] = x
matrixMul xs ys = [matrixMulOneRow x (transpose ys) | x <- xs]

matrixMulOneRow :: [Int] -> [[Int]] -> [Int]
matrixMulOneRow x ys = [scalarProduct x y | y <- ys]

and' :: [Bool] -> Bool
and' [] = False
and' [x] = x
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = replicateHelp' (n-1) a [a]

replicateHelp' :: Int -> a -> [a] -> [a]
replicateHelp' 0 _ xs = xs
replicateHelp' n a xs = replicateHelp' (n-1) a (a:xs)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = a == x || elem' a xs

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' [] ys = ys
merge' xs [] = xs
merge' (x:xs) (y:ys) | x <= y = x : merge' xs (y:ys)
                     | otherwise = y :  merge' (x:xs) ys

fun :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
fun f p xs = filter p (map f xs)

fun' :: (Num a) => (a -> a) -> (a -> Bool) -> [a] -> [a]
fun' f p xs = foldr (\x acc -> if p (f x) then f x : acc else acc) [] xs

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 xs = foldr (\(i, x) acc -> if even i then f1 x : acc else f2 x : acc) [] (zip [0..] xs)

represent :: (Eq a, Num a) => (a -> a) -> (a -> a)
represent _ 0 = 0
represent f x = f (x-1)