{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use infix" #-}

data NumExpr a = Value a
            | FromInteger Integer
            | Negate (NumExpr a)
            | Abs (NumExpr a)
            | SigNum (NumExpr a)
            | Add (NumExpr a) (NumExpr a)
            | Mul (NumExpr a) (NumExpr a)

instance Show a => Show(NumExpr a) where
  show (Value x)       = show x
  show (FromInteger n) = "fromInteger(" ++ show n ++ ")"
  show (Negate e)      = "negate(" ++ show e  ++ ")"
  show (Abs e)         = "abs(" ++ show e ++ ")"
  show (SigNum e)      = "signum(" ++ show e ++ ")"
  show (Add e e')      = show e ++ "+" ++ show e'
  show (Mul e e')      = pbo1 ++ show e ++ pbc1 ++ "*" ++ pbo2 ++ show e' ++ pbc2
                          where 
                            pbo1 = case e of 
                              (Mul n n') -> ""
                              (Value x) -> ""
                              _ -> "("
                            pbo2 = case e of 
                              (Mul n n') -> ""
                              (Value x) -> ""
                              _ -> "("
                            pbc1 = case e of 
                              (Mul n n') -> ""
                              (Value x) -> ""
                              _ -> ")"
                            pbc2 = case e of 
                              (Mul n n') -> ""
                              (Value x) -> ""
                              _ -> ")"
                            
ex1 :: NumExpr Int
ex1 = Abs $ Add (FromInteger 6) (Value 9)

ex2 :: NumExpr Int 
ex2 = Add (Add (Value 4) (Value 5)) (Add (Value 6) (Value 7)) 

ex3 :: NumExpr Int 
ex3 = Mul (Add (Value 4) (Value 5)) (Add (Value 6) (Value 7)) 

ex4 :: NumExpr Int 
ex4 = Add (Mul (Value 4) (Value 5)) (Add (Value 6) (Value 7))

data Value = BVal Bool
           | IVal Int
           deriving (Show,Eq)

data Expr = Val Value 
          | Var String
          | Plus Expr Expr
          | Times Expr Expr 
          | If Expr Expr Expr 
          | And Expr Expr
          | Or Expr Expr
          | Not Expr 
          | Lt Expr Expr
          deriving (Show,Eq)

type Env = String -> Maybe Value

exprEx :: Expr
exprEx = If (Lt (Var "x") (Val $ IVal 7)) (Plus (Var "x") (Val $ IVal 14)) (Val $ IVal 0) 
           
emptyEnv :: Env
emptyEnv _ = Nothing 

bind :: Env -> String -> Value -> Env 
bind env nm v nm' | nm == nm' = Just v
                  | otherwise = env nm' 

lookupEnv :: Env -> String -> Maybe Value
lookupEnv env nm = env nm 

eval :: Env -> Expr -> Maybe Value 
eval env (Val v) = Just v
eval env (Var s) = lookupEnv env s
eval env (Plus e1 e2) = case (eval env e1, eval env e2) of
                        (Just (IVal x), Just (IVal y)) -> Just (IVal (x + y))
                        (Nothing, Just (IVal y)) -> Just (IVal y)
                        _ -> Nothing 
eval env (Times e1 e2) = case (eval env e1, eval env e2) of
                        (Just (IVal x), Just (IVal y)) -> Just (IVal (x * y))
                        _ -> Nothing
eval env (If c e1 e2) = case (eval env c, eval env e1, eval env e2) of
                        (Just (BVal True), Just (IVal trueIVal), _) -> Just (IVal trueIVal)
                        (Just (BVal True), Just (BVal trueBVal), _) -> Just (BVal trueBVal)
                        (Just (BVal False), _, Just (IVal falseIVal)) -> Just (IVal falseIVal)
                        (Just (BVal False), _, Just (BVal falseBVal)) -> Just (BVal falseBVal)       
                        _ -> Nothing
eval env (And e1 e2) = case (eval env e1, eval env e2) of
                        (Just (BVal x), Just (BVal y)) -> Just (BVal (x && y)) 
                        _ -> Nothing
eval env (Or e1 e2) = case (eval env e1, eval env e2) of
                        (Just (BVal x), Just (BVal y)) -> Just (BVal (x || y)) 
                        _ -> Nothing
eval env (Not e1) = case eval env e1 of
                        (Just (BVal x)) -> Just (BVal (not x))
                        _ -> Nothing
eval env (Lt e1 e2) = case (eval env e1, eval env e2) of 
                        (Just (IVal x), Just (IVal y)) -> Just (BVal (x < y))
                        _ -> Nothing

deepExpr :: Expr
deepExpr =
  Plus
    (Plus
      (Plus (Val (IVal 1)) (Val (IVal 2)))
      (Plus (Val (IVal 3)) (Val (IVal 4))))
    (Plus
      (Var "x")
      (Plus (Val (IVal 5)) (Val (IVal 6))))


type Perm = [Int] 

isPerm :: [Int] -> Bool
isPerm ns = and [ elem k ns | k <- [0..length ns -1] ]

p' :: Perm
p' = [5,2,4,0,1,3]

q' :: Perm
q' = [7,3,2,0,8,9,1,4,6,5]

r :: [Int]
r = [0,3,2] 

cycleOf :: Int -> Perm -> [Int]
cycleOf n p = trackCycle n p (p !! n) [] 

trackCycle :: Int -> Perm -> Int -> [Int] -> [Int]
trackCycle n p i c | n == i = c ++ [n]
                   | otherwise = trackCycle n p (p !! i) (i : c)

cycles :: Perm -> [[Int]]
cycles p = findCycles p 0 []

findCycles :: Perm -> Int -> [[Int]] -> [[Int]]
findCycles p n xs | length p == n = xs
                  | otherwise = if checkIfSeen n xs then findCycles p (n+1) xs else findCycles p (n+1) (cycleOf n p : xs)

checkIfSeen :: Int -> [[Int]] -> Bool
checkIfSeen _ [] = False
checkIfSeen x (xs:xss) = x `elem` xs || checkIfSeen x xss