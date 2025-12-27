{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Char (toUpper)
 
orB :: Bool -> Bool -> Bool
orB False False = False
orB _ _ = True

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

removeFirstAndLast :: [a] -> [a]
removeFirstAndLast [] = []
removeFirstAndLast [x] = []
removeFirstAndLast (x:xs) = removeLast xs

removeLast :: [a] -> [a]
removeLast [] = []
removeLast [x] = []
removeLast (x:xs) = x : removeLast xs

conditionalReverse :: Int -> [a] -> [a]
conditionalReverse _ [] = []
conditionalReverse n xs | length xs >= n = reverse xs
                        | otherwise = xs

doubleFilter :: (Ord a, Num a) => [a] -> [a]
doubleFilter [] = []
doubleFilter (x:xs) | x <= 5 = doubleFilter xs
                    | otherwise = (2*x) : doubleFilter xs

reverseUppercase :: String -> String
reverseUppercase xs = map toUpper (reverse xs)

indexed :: [a] -> [(Int, a)]
indexed xs = zip [0..] xs

comparison :: Int -> Int -> Bool 
comparison x y | x > y && x <= (2*y) = True
               | otherwise = False

third :: [a] -> a
third xs = head $ tail $ tail xs 

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> Maybe a
third'' [] = Nothing 
third'' [x] = Nothing 
third'' [x, y] = Nothing
third'' (x:y:z:rest) = Just z

safeTail :: Eq a => [a] -> [a]
safeTail xs = if null xs then [] else tail xs

safeTail' :: Eq a => [a] -> [a]
safeTail' xs | null xs = []
             | otherwise = tail xs

safeTail'' :: Eq a => [a] -> [a]
safeTail'' [] = []
safeTail'' (x:xs) = xs