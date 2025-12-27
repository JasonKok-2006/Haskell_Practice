{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

data BST a = Empty | Branch a (BST a) (BST a)
    deriving Show

test :: BST Int
test = Empty

insertValueBST :: (Ord a, Num a) => BST a -> a -> BST a
insertValueBST Empty x = Branch x Empty Empty
insertValueBST (Branch n l r) x = if x <= n then Branch n (insertValueBST l x) r else Branch n l (insertValueBST r x)

insertValuesBST :: (Ord a, Num a) => BST a -> [a] -> BST a
insertValuesBST t xs = foldl insertValueBST t xs

isInTree :: (Ord a, Num a) => BST a -> a -> Bool
isInTree Empty _ = False
isInTree (Branch n l r) x | x == n = True
                          | x < n = isInTree l x
                          | otherwise = isInTree r x

isBST :: (Num a, Ord a) => BST a -> Bool
isBST Empty = True
isBST (Branch x l r) = checkBST l Nothing (Just x) && checkBST r (Just x) Nothing

checkBST :: (Num a, Ord a) => BST a -> Maybe a -> Maybe a -> Bool
checkBST Empty _ _ = True
checkBST (Branch val l r) (Just min) Nothing | min <= val = checkBST l (Just min) (Just val) && checkBST r (Just val) Nothing
                                             | otherwise = False
checkBST (Branch val l r) Nothing (Just max) | max >= val = checkBST l Nothing (Just val) && checkBST r (Just val) (Just max)
                                             | otherwise = False
checkBST (Branch val l r) (Just min) (Just max) | max >= val && min <= val = checkBST l (Just min) (Just val) && checkBST r (Just val) (Just max)
                                                | otherwise = False

data Rose a = BranchR a [Rose a] deriving Show

--type Direction = Int
-- type Address = [Direction]

testR :: Rose Int
testR = BranchR 1 [BranchR 2 [BranchR 6 [], BranchR 7 []], BranchR 3 [BranchR 8 []], BranchR 4 [], BranchR 5 [BranchR 9 [], BranchR 10 []]]

{-
-- These are 2 questions where the data types really don't make sense for the question. 
validAddresses :: Rose a -> [Address]
validAddresses (BranchR v vs) = undefined

getAtAddress :: Rose a -> Address -> Maybe a
getAtAddress = undefined
-}

data Tree a b = Leaf b | Fork (Tree a b) a (Tree a b)
  deriving (Eq, Show)

test2Bin :: Tree String Int 
test2Bin = Fork (Fork (Leaf 0) "Sam" (Leaf 1)) "Jason" (Fork (Leaf 2) "Bibi" (Leaf 3))

str2int :: String -> Int
str2int xs = length xs

int2bool :: Int -> Bool
int2bool n = n /= 0

applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns fa fb (Leaf b) = Leaf (fb b)
applyfuns fa fb (Fork l a r) = Fork (applyfuns fa fb l) (fa a) (applyfuns fa fb r)



data BinTree a = EmptyNode | Node (BinTree a) a (BinTree a)
  deriving (Eq, Show)

data Direction = GoLeft | GoRight
  deriving (Eq, Show, Bounded, Enum)

type Route = [Direction]

updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes [] f (Node l val r) = Node l (f val) r
updateNodes _ f EmptyNode = EmptyNode
updateNodes (p:ps) f (Node l val r) = case p of 
                                        GoLeft -> Node (updateNodes ps f l) (f val) r
                                        GoRight -> Node l (f val) (updateNodes ps f r)
