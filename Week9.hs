import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

facHelper :: Integer -> State Integer ()
facHelper 0 = pure ()
facHelper n = do
            facHelper (n-1)
            i <- get
            put (i*n) 

factorial :: Integer -> Integer
factorial n = execState (facHelper n) 1

data CalcExpr = Val Int
              | Add CalcExpr CalcExpr
              | Mult CalcExpr CalcExpr
              | Div CalcExpr CalcExpr
              | Sub CalcExpr CalcExpr

expr1 :: CalcExpr
expr1 = Mult (Add (Val 4) (Val 7)) (Div (Val 10) (Val 2))

expr2 :: CalcExpr
expr2 = Sub (Val 10) (Div (Val 14) (Val 0))

eval :: MonadError String m => CalcExpr -> m Int
eval (Val x) = pure x
eval (Add e1 e2) = do
                x <- eval e1
                y <- eval e2
                return (x+y)
eval (Mult e1 e2) = do
                x <- eval e1
                y <- eval e2
                return (x*y)
eval (Sub e1 e2) = do
                x <- eval e1
                y <- eval e2
                return (x-y)
eval (Div e1 e2) = do
                x <- eval e1
                y <- eval e2
                if y == 0
                    then throwError "Tried to divide by 0"
                    else return (x `div` y)

data CalcCmd = EnterC
             | StoreC Int CalcCmd
             | AddC Int CalcCmd
             | MultC Int CalcCmd
             | DivC Int CalcCmd
             | SubC Int CalcCmd

type CS a = StateT Int (Either String) a

cmd1 :: CalcCmd
cmd1 = StoreC 7 (AddC 14 (DivC 3 EnterC))

cmd2 :: CalcCmd
cmd2 = StoreC 10 (MultC 2 (DivC 0 EnterC))

run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run EnterC = return ()
run (StoreC n c) = do 
                    put n
                    run c
run (AddC n c) = do
                    x <- get
                    put (n + x)
                    run c
run (MultC n c) = do
                    x <- get
                    put (n * x)
                    run c
run (SubC n c) = do
                    x <- get
                    put (x - n)
                    run c
run (DivC n c) = do
                    x <- get
                    if n == 0
                        then throwError "Tried to divide by 0."
                        else put (x `div` n)
                    run c

data Dir = File String String
         | SubDir String [Dir]
         deriving Show

recipes :: Dir
recipes = SubDir "Recipes" [ SubDir "Tex-Mex" [ File "Tacos" "meat, cheese, tomato"
                                              , File "Burrito" "tortilla, rice, beans"
                                              ]
                           , SubDir "Italian" [ File "Pizza" "dough, sauce, pepperoni"
                                              , File "Bolognese" "pasta, ground beef, tomato sauce"
                                              ]
                           , SubDir "French"  [ File "Ratatouille" "tomato, bell peppers, eggplant"
                                              , File "Croque Monsieur" "toast, cheese, ham"
                                              ]
                           ]

logTraverse :: Dir -> Writer [String] ()
logTraverse (File s _) = tell ["Passing file: " ++ s]
logTraverse (SubDir s subs) = do
                                tell ["Entering directory: " ++ s]
                                mapM_ logTraverse (reverse subs)
                                tell ["Leaving directory: " ++ s]

countFiles :: Dir -> State Int ()
countFiles (File _ _) = do 
                        i <- get
                        put (i+1)
countFiles (SubDir _ subs) = do
                        mapM_ countFiles subs