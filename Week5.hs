import Data.Char ( toUpper, toLower )
import Data.Maybe (fromJust)
import GHC.Unicode (isUpperCase)
import GHC.Base (DoubleBox)
type Button = Char
type Presses = Int
type Text = String

output :: (Button, Presses) -> Text
output ('0', 1) = " "
output ('0', 0) = "0"
output ('1', 0) = "1"
output ('2', 1) = "a"
output ('2', 2) = "b"
output ('2', 3) = "c"
output ('2', 0) = "2"
output ('3', 1) = "d"
output ('3', 2) = "e"
output ('3', 3) = "f"
output ('3', 0) = "3"
output ('4', 1) = "g"
output ('4', 2) = "h"
output ('4', 3) = "i"
output ('4', 0) = "4"
output ('5', 1) = "j"
output ('5', 2) = "k"
output ('5', 3) = "l"
output ('5', 0) = "5"
output ('6', 1) = "m"
output ('6', 2) = "n"
output ('6', 3) = "o"
output ('6', 0) = "6"
output ('7', 1) = "p"
output ('7', 2) = "q"
output ('7', 3) = "r"
output ('7', 4) = "s"
output ('7', 0) = "7"
output ('8', 1) = "t"
output ('8', 2) = "u"
output ('8', 3) = "v"
output ('8', 0) = "8"
output ('9', 1) = "w"
output ('9', 2) = "x"
output ('9', 3) = "y"
output ('9', 4) = "z"
output ('9', 0) = "9"
output ('#', 1) = "."
output ('#', 0) = ","
output ('*', 1) = "upper"
output ('*', 0) = "lower"
output _ = "?"

type Table = [((Button, Presses), Text)]

messageTable :: Table
messageTable = [ ((c, i), output (c, i)) | c <- ['0'..'9'] ++ ['#', '*'] , i <- [0..4] ]

phoneToString :: [(Button, Presses)] -> Text
phoneToString [] = ""
phoneToString ((b, p):rest) | b == '*' && p `rem` 2 == 1 = case rest of
                                                                [] -> ""
                                                                ((b',p'):rest') -> map toUpper (fromJust (lookup (b', p' `rem` opt b') messageTable)) ++ phoneToString rest'
                            |  b == '*' && even p = case rest of
                                                                [] -> ""
                                                                ((b',p'):rest') -> fromJust (lookup (b', p' `rem` opt b') messageTable) ++ phoneToString rest'
                            | otherwise = fromJust (lookup (b, p `rem` opt b) messageTable) ++ phoneToString rest

opt :: Button -> Presses
opt '0' = 2
opt '1' = 1
opt '2' = 4
opt '3' = 4
opt '4' = 4
opt '5' = 4
opt '6' = 4
opt '7' = 5
opt '8' = 4
opt '9' = 5
opt '#' = 2
opt '*' = 2
opt _   = 1

lookupByValue :: Text -> Table -> Maybe (Button, Presses)
lookupByValue _ [] = Nothing
lookupByValue s (((b, p), t):rest) = if s == t then Just (b, p) else lookupByValue s rest

stringToPhone :: Text -> [(Button, Presses)]
stringToPhone [] = []
stringToPhone (x:xs) = if isUpperCase x then ('*', 1) : stringToPhone (toLower x : xs) else fromJust (lookupByValue [x] messageTable) : stringToPhone xs

fingerTaps :: Text -> Presses
fingerTaps s = totalTaps (stringToPhone s)

totalTaps :: [(Button, Presses)] -> Presses
totalTaps [] = 0
totalTaps ((b,p):xs) = case p of
                    0 -> opt b + totalTaps xs
                    n -> p + totalTaps xs

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

tailMaybe :: [a] -> Maybe [a]
tailMaybe [] = Nothing
tailMaybe [x] = Nothing
tailMaybe (x:xs) = Just xs

takeMaybe :: Int -> [a] -> Maybe [a]
takeMaybe 0 _ = Nothing
takeMaybe _ [] = Nothing
takeMaybe n xs = if n > length xs then Nothing else Just (take n xs)

zipEither :: [a] -> [b] -> Either String [(a,b)]
zipEither [] [] = Right []
zipEither _ [] = Left "Forst list is empty."
zipEither [] _ = Left "Second list is empty."
zipEither xs ys | length xs == length ys = Right (zip xs ys)
                | length xs < length ys = Left "First list is smaller than the second."
                | otherwise = Left "Second list is smaller than the second."

toList :: Maybe a -> [a]
toList Nothing = []
toList (Just x) = [x]

toMaybe :: [a] -> Maybe a
toMaybe [] = Nothing
toMaybe [x] = Just x



data BinLN a b = Empty | Lf a | Nd b (BinLN a b) (BinLN a b)
    deriving Show

t3 :: BinLN Int Int
t3 = Nd 10 (Nd 5 (Lf 2) (Nd 8 (Lf 3) (Lf 4))) (Nd 8 (Lf 5) (Lf 6))

t4 :: BinLN Int Int
t4 = Nd 11 (Nd 15 (Lf 12) (Lf 13)) (Lf 21)

leaves :: BinLN a b -> [a]
leaves (Lf a) = [a]
leaves (Nd b l r) = leaves l ++ leaves r

data BinRose a = Leaf a | Node (BinRose a) (BinRose a)
    deriving Show

rt :: BinRose Int
rt = Node (Leaf 10) (Node (Node (Leaf 1) (Leaf 3)) (Leaf 19))

showBinRose :: Show a => BinRose a -> String
showBinRose (Leaf a) = show a
showBinRose (Node l r) = "(" ++ showBinRose l ++ showBinRose r ++ ")"

(\\) :: BinLN a a -> BinLN a a -> BinLN a a 
(\\) (Lf a) s = Nd a Empty s
(\\) (Nd a l r) s = Nd a l ((\\) r s)

(//) :: BinLN a a -> BinLN a a -> BinLN a a
(//) (Lf a) s = Nd a s Empty
(//) (Nd a l r) s = Nd a ((\\) l s) r

leafIndices :: BinLN a a -> BinLN (Int,Int) (Int, Int)
leafIndices t = setNodes $ setLeaves t 0

countLeaves ::  BinLN a a  -> Int 
countLeaves (Lf a) = 1
countLeaves (Nd a l r) = 0 + countLeaves l + countLeaves r

setLeaves :: BinLN a a -> Int -> BinLN (Int,Int) (Int, Int)
setLeaves Empty _ = Empty
setLeaves (Lf a) n = Lf (n, n)
setLeaves (Nd a l r) n = Nd (0, 0) (setLeaves l n) (setLeaves r (n + countLeaves l))

setNodes :: BinLN (Int, Int) (Int, Int) -> BinLN (Int, Int) (Int, Int)
setNodes (Lf (n, m)) = Lf (n, m)
setNodes (Nd (nl, nr) l r) = Nd (deepestLeftLeaf l, deepestRightLeaf r) (setNodes l) (setNodes r)

deepestLeftLeaf :: BinLN (Int, Int) (Int, Int) -> Int
deepestLeftLeaf (Lf (n, m)) = n
deepestLeftLeaf (Nd _ l _) = deepestLeftLeaf l

deepestRightLeaf :: BinLN (Int, Int) (Int, Int) -> Int
deepestRightLeaf (Lf (n, m)) = n
deepestRightLeaf (Nd _ _ r) = deepestRightLeaf r