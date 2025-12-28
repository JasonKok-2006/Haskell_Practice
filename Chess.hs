-- Welcome to my attempt at coding chess within haskell.

-- Warning ignorations set throughout the project.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.ByteString (find)
import Data.IntMap (insert)
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use foldr" #-}

-- Generating the game board and setting the squares
type GameBoard = [[Cell]]
type Cell = (Char, Int)

generateGameBoard :: [[Cell]]
generateGameBoard = split [ (c, i) | i <- (reverse [1..8] :: [Int]), c <- ['A'..'H'] ]

split :: [a] -> [[a]]
split [] = []
split xs = take 8 xs : split (drop 8 xs)

-- Making the pieces
type Moves = Int

data Piece a b c = Pawn Players Moves Cell
                | Knight Players Moves Cell
                | Bishop Players Moves Cell
                | Rook Players Moves Cell
                | Queen Players Moves Cell
                | King Players Moves Cell

-- Players Code
data Players = White | Black
    deriving Show

selectPlayer :: Int -> Players
selectPlayer n = if even n then White else Black

-- Pieces code
type Pieces = [Piece Players Moves Cell]
type GameState = ([[(Cell, Maybe (Piece Players Moves Cell))]], Moves)

instance Show (Piece Players Moves Cell) where
    show (Pawn White _ _) = "P"
    show (Pawn Black _ _) = "p"
    show (Knight White _ _) = "N"
    show (Knight Black _ _) = "n"
    show (Bishop White _ _) = "B"
    show (Bishop Black _ _) = "b"
    show (Rook White _ _) = "R"
    show (Rook Black _ _) = "r"
    show (Queen White _ _) = "Q"
    show (Queen Black _ _) = "q"
    show (King White _ _) = "K"
    show (King Black _ _) = "k"

-- Initilal Board
initialPositions :: Pieces
initialPositions = [Rook Black 0 ('A',8),
                    Knight Black 0 ('B', 8),
                    Bishop Black 0 ('C', 8),
                    Queen Black 0 ('D', 8),
                    King Black 0  ('E', 8),
                    Bishop Black 0 ('F', 8),
                    Knight Black 0 ('G', 8),
                    Rook Black 0 ('H', 8),
                    Pawn Black 0 ('A', 7),
                    Pawn Black 0 ('B', 7),
                    Pawn Black 0 ('C', 7),
                    Pawn Black 0 ('D', 7),
                    Pawn Black 0 ('E', 7),
                    Pawn Black 0 ('F', 7),
                    Pawn Black 0 ('G', 7),
                    Pawn Black 0 ('H', 7),
                    Pawn White 0 ('A', 2),
                    Pawn White 0 ('B', 2),
                    Pawn White 0 ('C', 2),
                    Pawn White 0 ('D', 2),
                    Pawn White 0 ('E', 2),
                    Pawn White 0 ('F', 2),
                    Pawn White 0 ('G', 2),
                    Pawn White 0 ('H', 2),
                    Rook White 0 ('A',1),
                    Knight White 0 ('B', 1),
                    Bishop White 0 ('C', 1),
                    Queen White 0 ('D', 1),
                    King White 0 ('E', 1),
                    Bishop White 0 ('F', 1),
                    Knight White 0 ('G', 1),
                    Rook White 0 ('H',1)]

-- Create a board state
createGame :: GameBoard -> Pieces -> GameState
createGame board pieces = (split (createGameState board pieces), 0)

createGameState :: GameBoard -> Pieces -> [(Cell, Maybe (Piece Players Moves Cell))]
createGameState [] _ = []
createGameState (x:xs) ys = createGameStateRow x ys ++ createGameState xs ys

createGameStateRow :: [Cell] -> Pieces -> [(Cell, Maybe (Piece Players Moves Cell))]
createGameStateRow [] _ = []
createGameStateRow (x:xs) ys = insertPieces x ys : createGameStateRow xs ys

insertPieces :: Cell -> Pieces -> (Cell, Maybe (Piece Players Moves Cell))
insertPieces x [] = (x, Nothing)
insertPieces x (p:ps) | checkPiece x p = (x, Just p)
                      | otherwise = insertPieces x ps

checkPiece :: Cell -> Piece Players Moves Cell -> Bool
checkPiece x (Pawn _ _ c) = x == c
checkPiece x (Knight _ _ c) = x == c
checkPiece x (Bishop _ _ c) = x == c
checkPiece x (Rook _ _ c) = x == c
checkPiece x (Queen _ _ c) = x == c
checkPiece x (King _ _ c) = x == c

rowDivider :: String
rowDivider = " ___ ___ ___ ___ ___ ___ ___ ___ "

rowDividerAlt :: String
rowDividerAlt = "|___|___|___|___|___|___|___|___|"

-- Showing the Game State
printGameState :: GameState -> IO ()
printGameState gs = putStrLn (rowDivider++ showGameState gs)

showGameState :: GameState -> String
showGameState ([], _) = ""
showGameState (x:xs, moves) = "\n" ++ showRow x ++ rowDividerAlt ++ showGameState (xs, moves)

showRow :: [(Cell, Maybe (Piece Players Moves Cell))] -> String
showRow [] = "|\n"
showRow (x:xs) = showCell x ++ showRow xs

showCell :: (Cell, Maybe (Piece Players Moves Cell)) -> String
showCell (cell, Nothing) = "|   "
showCell (cell, Just piece) = "| " ++ show piece ++ " "

initial :: GameState
initial = createGame generateGameBoard initialPositions

-- Code where given a move, the game state is updated accordingly
updateGameState :: GameState -> Cell -> Cell -> GameState
updateGameState gs from to = case pickUpPiece gs from of
                                (Left piece, (board', moves)) -> (if validCell to then insertPiece (board', moves+1) to piece else gs) -- If invalid cell, return the same game state
                                (Right msg, gs') -> gs -- If no piece found, return the same game state

pickUpPiece :: GameState -> Cell -> (Either (Piece Players Moves Cell) String, GameState)
pickUpPiece gs cell = case storePiece gs cell of
                        Right m -> (Right m, gs)
                        Left p -> (Left p, removePieceFromSquare gs cell)

removePieceFromSquare :: GameState -> Cell -> GameState
removePieceFromSquare ([], moves) _ = ([], moves)
removePieceFromSquare (board, moves) cell | ry == ry' = (fst (editRow (row, moves) cell) : rest, moves)
                                          | otherwise = (row : fst (removePieceFromSquare (rest, moves) cell), moves)
    where
        (row:rest) = board -- Split the board into the first row and the rest
        (c:cs) = row -- split the row into the first celland the rest
        (cx, ry) = fst c -- split the cell into the tuple that holds the column and row
        (cx', ry') = cell -- split the target cell into the tuple that holds the column and row

editRow :: ([(Cell, Maybe (Piece Players Moves Cell))], Moves) -> Cell -> ([(Cell, Maybe (Piece Players Moves Cell))], Moves)
editRow ([], moves) _ = ([], moves)
editRow (row, moves) cell | cx == cx' = ((fst c, Nothing) : cs, moves)
                          | otherwise = (c : fst (editRow (cs, moves) cell), moves)
    where
        (c:cs) = row -- split the row into the first celland the rest
        (cx, ry) = fst c -- split the cell into the tuple that holds the column and row
        (cx', ry') = cell -- split the target cell into the tuple that holds the column and row
        piece = snd c -- get the piece at the current cell 

storePiece :: GameState -> Cell -> Either (Piece Players Moves Cell) String
storePiece ([], moves) _ = Right "Cell not found"
storePiece (board, moves) cell | ry == ry' = findPiece row cell
                               | otherwise = storePiece (rest, moves) cell
    where
        (row:rest) = board -- Split the board into the first row and the rest
        (c:cs) = row -- split the row into the first celland the rest
        (cx, ry) = fst c -- split the cell into the tuple that holds the column and row
        (cx', ry') = cell -- split the target cell into the tuple that holds the column and row

findPiece :: [(Cell, Maybe (Piece Players Moves Cell))] -> Cell -> Either (Piece Players Moves Cell) String
findPiece [] _ = Right "Cell not found"
findPiece row cell = if cx == cx'
                        then
                            case piece of
                                Just p -> Left p
                                Nothing -> Right "No piece found at first cell"
                        else findPiece cs cell
    where
        (c:cs) = row -- split the row into the first celland the rest
        (cx, ry) = fst c -- split the cell into the tuple that holds the column and row
        (cx', ry') = cell -- split the target cell into the tuple that holds the column and row
        piece = snd c -- get the piece at the current cell 

insertPiece :: GameState -> Cell -> Piece Players Moves Cell -> GameState
insertPiece ([], moves) _ _ = ([], moves)
insertPiece (board, moves) cell piece | ry == ry' = (fst (editRowInsert (row, moves) cell piece) : rest, moves)
                                      | otherwise = (row : fst (insertPiece (rest, moves) cell piece), moves)
    where
        (row:rest) = board -- Split the board into the first row and the rest
        (c:cs) = row -- split the row into the first celland the rest
        (cx, ry) = fst c -- split the cell into the tuple that holds the column and row
        (cx', ry') = cell -- split the target cell into the tuple that holds the column and row

editRowInsert :: ([(Cell, Maybe (Piece Players Moves Cell))], Moves) -> Cell -> Piece Players Moves Cell -> ([(Cell, Maybe (Piece Players Moves Cell))], Moves)
editRowInsert ([], moves) _ _ = ([], moves)
editRowInsert (row, moves) cell piece | cx == cx' = ((fst c, Just piece) : cs, moves)
                                      | otherwise = (c : fst (editRowInsert (cs, moves) cell piece), moves)
    where
        (c:cs) = row -- split the row into the first celland the rest
        (cx, ry) = fst c -- split the cell into the tuple that holds the column and row
        (cx', ry') = cell -- split the target cell into the tuple that holds the column and row 

validCell :: Cell -> Bool
validCell (c, r) = c `elem` ['A'..'H'] && r `elem` [1..8]