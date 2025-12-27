{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldl" #-}

(!?) :: [a] -> Integer -> Maybe a
(!?) [] _ = Nothing
(!?) (x:xs) n | n == 0 = Just x
              | otherwise = (!?) xs (n-1)

(!!!) :: [a] -> Integer -> a
(!!!) [] _ = undefined
(!!!) (x:xs) n | n == 0 = x
               | otherwise = (!!!) xs (n-1)

int2int :: Integer -> Maybe Integer
int2int = Just

int2char :: Integer -> Maybe Char
int2char x = (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']) !? x -- Only 62 Charcaters (range 0 - 61)

int2charJ :: Integer -> Char
int2charJ x = (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']) !!! x -- Only 62 Charcaters (range 0 - 61)

string2mstring :: Integer -> String
string2mstring x | x <= 0 = ""
                 | otherwise =  int2charJ (x `rem` 62) : string2mstring (x `div` 62)

int2float :: Integer -> Maybe Float
int2float x = Just (0.0001 * fromIntegral x)

int2string :: Integer -> Maybe String
int2string x = Just (string2mstring x)

equalBinFunctions :: Eq b => [Integer] -> (Integer -> Maybe a) -> (a -> b) -> (a -> b) -> Bool
equalBinFunctions [] _ _ _ = True
equalBinFunctions (x:xs) fc f1 f2 = case fc x of
                                        (Just t) -> (f1 t == f2 t) && equalBinFunctions xs fc f1 f2
                                        Nothing -> equalBinFunctions xs fc f1 f2

myReverse :: [a] -> [a]
myReverse xs = myReverseHelp xs []

myReverseHelp :: [a] -> [a] -> [a]
myReverseHelp [] ys = ys
myReverseHelp (x:xs) ys = myReverseHelp xs (x : ys)


{-

Check if functions are equal:
    Boolean values -> True or False

Can you do this?
Integer, String (Words) Values -> No

To check if function are equal, you have to check all cases. 

How many cases are there for Integers (and Strings)?
Infinite

To check equal functions in this case is known as Semi-decidable.
We can only prove one way, Not the second. 

We can tell they are not equal, if there is a value where (f1 x) != (f2 x) 
If we want to know if they are full equal, we have infinite test cases. Which goes on forever. Hence we can't fully know if 2 functions are equal. 

We can. Because +0 and *1 are.

-}