-- Welcome to my attempt at coding chess within haskell.

-- Warning ignorations set throughout the project.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use record patterns" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant if" #-}
import Data.ByteString (find)
import Data.IntMap (insert)
import System.FilePath (isValid)
import Data.Map (valid)
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
    deriving (Show, Eq)

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
updateGameState gs from to = if isValidMove gs from to
                                then case pickUpPiece gs from of
                                    (Left piece, (board', moves)) -> (if validCell to then insertPiece (board', moves) to piece else gs) -- If invalid cell, return the same game state
                                    (Right msg, gs') -> gs -- If no piece found, return the same game state
                                else gs -- If invalid move, return the same game state

nextTurn :: GameState -> GameState
nextTurn (board, moves) = (board, moves + 1)

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
insertPiece (board, moves) cell piece | ry == ry' = (fst (editRowInsert (row, moves) cell (createPiece (board, moves) piece cell)) : rest, moves)
                                      | otherwise = (row : fst (insertPiece (rest, moves) cell piece), moves)
    where
        (row:rest) = board -- Split the board into the first row and the rest
        (c:cs) = row -- split the row into the first celland the rest
        (cx, ry) = fst c -- split the cell into the tuple that holds the column and row
        (cx', ry') = cell -- split the target cell into the tuple that holds the column and row

createPiece :: GameState -> Piece Players Moves Cell -> Cell -> Piece Players Moves Cell
createPiece (board, moves) piece cell = case piece of
                                        Pawn player _ _ -> Pawn player moves cell
                                        Knight player _ _ -> Knight player moves cell
                                        Bishop player _ _ -> Bishop player moves cell
                                        Rook player _ _ -> Rook player moves cell
                                        Queen player _ _ -> Queen player moves cell
                                        King player _ _ -> King player moves cell

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

-- Logic for validating whether the piece belongs to the current player's turn and checking valid moves
isValidMove :: GameState -> Cell -> Cell -> Bool
isValidMove (board, moves) from to = case pickUpPiece (board, moves) from of
                                        (Left piece, _) -> belongsToCurrentPlayer (board, moves) piece && to `elem` validMoves (board, moves) piece from
                                        (Right _, _) -> False

belongsToCurrentPlayer :: GameState -> Piece Players Moves Cell -> Bool
belongsToCurrentPlayer (board, moves) piece = case piece of
                                                Pawn player _ _ -> player == selectPlayer moves
                                                Knight player _ _ -> player == selectPlayer moves
                                                Bishop player _ _ -> player == selectPlayer moves
                                                Rook player _ _ -> player == selectPlayer moves
                                                Queen player _ _ -> player == selectPlayer moves
                                                King player _ _ -> player == selectPlayer moves

validMoves :: GameState -> Piece Players Moves Cell -> Cell -> [Cell]
validMoves gs piece cell = case piece of
                                    Pawn player _ _ -> validPawnMoves gs piece cell
                                    Knight _ _ _ -> validKnightMovesSetup gs cell
                                    Bishop _ _ _ -> validMovesDiagonalUpLeft gs cell ++ validMovesDiagonalUpRight gs cell ++ validMovesDiagonalDownLeft gs cell ++ validMovesDiagonalDownRight gs cell
                                    Rook _ _ _ -> validMovesHorizontalLeft gs cell ++ validMovesHorizontalRight gs cell ++ validMovesVerticalUp gs cell ++ validMovesVerticalDown gs cell
                                    Queen _ _ _ -> validMovesHorizontalLeft gs cell ++ validMovesHorizontalRight gs cell ++ validMovesVerticalUp gs cell ++ validMovesVerticalDown gs cell ++ validMovesDiagonalUpLeft gs cell ++ validMovesDiagonalUpRight gs cell ++ validMovesDiagonalDownLeft gs cell ++ validMovesDiagonalDownRight gs cell
                                    King _ _ _ -> [] -- King movement not implemented yet

-- This is a wierd but necessary function to change characters by an integer value
changingChar :: Char -> Int -> Char
changingChar c n = toEnum (fromEnum c + n)

validMovesHorizontalLeft :: GameState -> Cell -> [Cell]
validMovesHorizontalLeft gs (c, r) = if validCell (changingChar c (-1), r)
                                    then (case storePiece gs (changingChar c (-1), r) of
                                        Right _ -> (changingChar c (-1), r) : validMovesHorizontalLeft gs (changingChar c (-1), r)
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then []
                                            else [(changingChar c (-1), r)])
                                    else []

validMovesHorizontalRight :: GameState -> Cell -> [Cell]
validMovesHorizontalRight gs (c, r) = if validCell (changingChar c 1, r)
                                    then (case storePiece gs (changingChar c 1, r) of
                                        Right _ -> (changingChar c 1, r) : validMovesHorizontalRight gs (changingChar c 1, r)
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then []
                                            else [(changingChar c 1, r)])
                                    else []

validMovesVerticalUp :: GameState -> Cell -> [Cell]
validMovesVerticalUp gs (c, r) = if validCell (c, r + 1)
                                    then (case storePiece gs (c, r + 1) of
                                        Right _ -> (c, r + 1) : validMovesVerticalUp gs (c, r + 1)
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then []
                                            else [(c, r + 1)])
                                    else []

validMovesVerticalDown :: GameState -> Cell -> [Cell]
validMovesVerticalDown gs (c, r) = if validCell (c, r - 1)
                                    then (case storePiece gs (c, r - 1) of
                                        Right _ -> (c, r - 1) : validMovesVerticalDown gs (c, r - 1)
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then []
                                            else [(c, r - 1)])
                                    else []

validMovesDiagonalUpLeft :: GameState -> Cell -> [Cell]
validMovesDiagonalUpLeft gs (c, r) = if validCell (changingChar c (-1), r + 1)
                                    then (case storePiece gs (changingChar c (-1), r + 1) of
                                        Right _ -> (changingChar c (-1), r + 1) : validMovesDiagonalUpLeft gs (changingChar c (-1), r + 1)
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then []
                                            else [(changingChar c (-1), r + 1)])
                                    else []

validMovesDiagonalUpRight :: GameState -> Cell -> [Cell]
validMovesDiagonalUpRight gs (c, r) = if validCell (changingChar c 1, r + 1)
                                    then (case storePiece gs (changingChar c 1, r + 1) of
                                        Right _ -> (changingChar c 1, r + 1) : validMovesDiagonalUpRight gs (changingChar c 1, r + 1)
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then []
                                            else [(changingChar c 1, r + 1)])
                                    else []

validMovesDiagonalDownLeft :: GameState -> Cell -> [Cell]
validMovesDiagonalDownLeft gs (c, r) = if validCell (changingChar c (-1), r - 1)
                                    then (case storePiece gs (changingChar c (-1), r - 1) of
                                        Right _ -> (changingChar c (-1), r - 1) : validMovesDiagonalDownLeft gs (changingChar c (-1), r - 1)
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then []
                                            else [(changingChar c (-1), r - 1)])
                                    else []

validMovesDiagonalDownRight :: GameState -> Cell -> [Cell]
validMovesDiagonalDownRight gs (c, r) = if validCell (changingChar c 1, r - 1)
                                    then (case storePiece gs (changingChar c 1, r - 1) of
                                        Right _ -> (changingChar c 1, r - 1) : validMovesDiagonalDownRight gs (changingChar c 1, r - 1)
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then []
                                            else [(changingChar c 1, r - 1)])
                                    else []

validKnightMovesSetup :: GameState -> Cell -> [Cell]
validKnightMovesSetup gs (c, r) = validKnightMoves gs [(changingChar c 1, r + 2),
                                                       (changingChar c 2, r + 1),
                                                       (changingChar c 2, r - 1),
                                                       (changingChar c 1, r - 2),
                                                       (changingChar c (-1), r - 2),
                                                       (changingChar c (-2), r - 1),
                                                       (changingChar c (-2), r + 1),
                                                       (changingChar c (-1), r + 2)]

validKnightMoves :: GameState -> [Cell] -> [Cell]
validKnightMoves gs [] = []
validKnightMoves gs ((c, r): rest) = if validCell (c, r)
                                    then (case storePiece gs (c, r) of
                                        Right _ -> (c, r) : validKnightMoves gs rest
                                        Left p -> if belongsToCurrentPlayer gs p
                                            then validKnightMoves gs rest
                                            else (c, r) : validKnightMoves gs rest)
                                    else validKnightMoves gs rest

validPawnMoves :: GameState -> Piece Players Moves Cell -> Cell -> [Cell]
validPawnMoves gs (Pawn White 0 _) (c, r) = diagonalAttack gs (c, r) ++ case storePiece gs (c, r + 1) of
                                            Right _ -> (c, r + 1) : case storePiece gs (c, r + 2) of
                                                                    Right _ -> [(c, r + 2)]
                                                                    Left p -> []
                                            Left p -> []
validPawnMoves gs (Pawn White _ _) (c, r) = diagonalAttack gs (c, r) ++ case storePiece gs (c, r + 1) of
                                            Right _ -> [(c, r + 1)]
                                            Left p -> []
validPawnMoves gs (Pawn Black 0 _) (c, r) = diagonalAttackBlack gs (c, r) ++ case storePiece gs (c, r - 1) of
                                            Right _ -> (c, r - 1) : case storePiece gs (c, r - 2) of
                                                                    Right _ -> [(c, r - 2)]
                                                                    Left p -> []
                                            Left p -> []
validPawnMoves gs (Pawn Black _ _) (c, r) = diagonalAttackBlack gs (c, r) ++ case storePiece gs (c, r - 1) of
                                            Right _ -> [(c, r - 1)]
                                            Left p -> []

diagonalAttack :: GameState -> Cell -> [Cell]
diagonalAttack gs (c, r) = pawnAttackLeft gs (c, r) ++ pawnAttackRight gs (c, r)

pawnAttackRight :: GameState -> Cell -> [Cell]
pawnAttackRight (board, moves) (c, r) = case storePiece (board, moves) (changingChar c 1, r + 1) of
                                    Right _ -> case storePiece (board, moves) (changingChar c 1, r) of -- en passant rule
                                            Right _ -> []
                                            Left p' -> if r == 6 && opponentPawnFirstMove (board, moves) p' (changingChar c 1, r)
                                                then [(changingChar c 1, r + 1)]
                                                else []
                                    Left p -> if not (belongsToCurrentPlayer (board, moves) p)
                                                then [(changingChar c 1, r + 1)]
                                                else []

pawnAttackLeft :: GameState -> Cell -> [Cell]
pawnAttackLeft (board, moves) (c, r) = case storePiece (board, moves) (changingChar c (-1), r + 1) of
                                    Right _ -> case storePiece (board, moves) (changingChar c (-1), r) of -- en passant rule
                                            Right _ -> []
                                            Left p' -> if r == 6 && opponentPawnFirstMove (board, moves) p' (changingChar c (-1), r)
                                                then [(changingChar c (-1), r + 1)]
                                                else []
                                    Left p -> if not (belongsToCurrentPlayer (board, moves) p)
                                                then [(changingChar c (-1), r + 1)]
                                                else []

opponentPawnFirstMove :: GameState -> Piece Players Moves Cell -> Cell -> Bool
opponentPawnFirstMove gameState piece cell = case storePiece gameState cell of
                                                Right _ -> False
                                                Left p -> case p of
                                                            Pawn player pieceMoves _ -> not (belongsToCurrentPlayer gameState p) && moves - pieceMoves == 1
                                                            _ -> False
    where
        (board, moves) = gameState
        (c, r) = cell

diagonalAttackBlack :: GameState -> Cell -> [Cell]
diagonalAttackBlack gs (c, r) = pawnAttackLeftBlack gs (c, r) ++ pawnAttackRightBlack gs (c, r)

pawnAttackRightBlack :: GameState -> Cell -> [Cell]
pawnAttackRightBlack (board, moves) (c, r) = case storePiece (board, moves) (changingChar c 1, r - 1) of
                                    Right _ -> case storePiece (board, moves) (changingChar c 1, r) of -- en passant rule
                                            Right _ -> []
                                            Left p' -> if r == 3 && opponentPawnFirstMove (board, moves) p' (changingChar c 1, r)
                                                then [(changingChar c 1, r - 1)]
                                                else []
                                    Left p -> if not (belongsToCurrentPlayer (board, moves) p)
                                                then [(changingChar c 1, r - 1)]
                                                else []

pawnAttackLeftBlack :: GameState -> Cell -> [Cell]
pawnAttackLeftBlack (board, moves) (c, r) = case storePiece (board, moves) (changingChar c (-1), r - 1) of
                                    Right _ -> case storePiece (board, moves) (changingChar c (-1), r) of
                                            Right _ -> []
                                            Left p' -> if r == 3 && opponentPawnFirstMove (board, moves) p' (changingChar c (-1), r)
                                                then [(changingChar c (-1), r - 1)]
                                                else []
                                    Left p -> if not (belongsToCurrentPlayer (board, moves) p)
                                                then [(changingChar c (-1), r - 1)]
                                                else []

-- check and checkmate
-- Checking for check
moveResultsInCheck :: GameState -> Cell -> Bool
moveResultsInCheck gs from = canReachKing gs (storePiece gs from) from (findOpponentKing gs)

canReachKing :: GameState -> Either (Piece Players Moves Cell) String -> Cell -> Cell -> Bool
canReachKing gs piece from to = case piece of
                                    Left p -> to `elem` validMoves gs p from
                                    Right _ -> False

findKing :: GameState -> Cell
findKing ([], _) = error "King not found"
findKing (board, moves) | fst (kingInRow row moves) = snd (kingInRow row moves)
                        | otherwise = findKing (rest, moves)
    where
        (row:rest) = board -- Split the board into the first row and the rest

kingInRow :: [(Cell, Maybe (Piece Players Moves Cell))] -> Moves -> (Bool, Cell)
kingInRow [] _ = (False, ('Z', 0)) -- This case should never happen
kingInRow (x:xs) moves = case snd x of
                    Just (King player _ _) -> if player == selectPlayer moves then (True, fst x) else kingInRow xs moves
                    _ -> kingInRow xs moves

findOpponentKing :: GameState -> Cell
findOpponentKing ([], _) = error "King not found"
findOpponentKing (board, moves) | fst (opponentKingInRow row moves) = snd (opponentKingInRow row moves)
                                | otherwise = findOpponentKing (rest, moves)
    where
        (row:rest) = board -- Split the board into the first row and the rest

opponentKingInRow :: [(Cell, Maybe (Piece Players Moves Cell))] -> Moves -> (Bool, Cell)
opponentKingInRow [] _ = (False, ('Z', 0)) -- This case should never happen
opponentKingInRow (x:xs) moves = case snd x of
                    Just (King player _ _) -> if player /= selectPlayer moves then (True, fst x) else opponentKingInRow xs moves
                    _ -> opponentKingInRow xs moves

-- Check for if a move would reult in check on your own king
-- Check the opponent king as we already made the move and the turn has changed
discoveredCheck :: GameState -> Cell -> Bool
discoveredCheck gs pieceCell = isItInLineWithKing gs pieceCell && newCheckDetected gs direction
    where
        kingCell = findKing gs
        positionDifference = decipherPositionDifference kingCell pieceCell
        direction = decipherDirectionDifference positionDifference

isItInLineWithKing :: GameState -> Cell -> Bool
isItInLineWithKing gs pieceCell = pieceCell `elem` validMoves gs (Queen (selectPlayer (snd gs)) (snd gs) kingCell) kingCell
    where
        kingCell = findKing gs

newCheckDetected :: GameState -> Int -> Bool
newCheckDetected gs 1 = straightThreatDetected gs moveVertically (findKing gs) 1 -- vertical up
newCheckDetected gs 2 = straightThreatDetected gs moveVertically (findKing gs) (-1) -- vertical down
newCheckDetected gs 3 = straightThreatDetected gs moveHorizontally (findKing gs) 1 -- horizontal right
newCheckDetected gs 4 = straightThreatDetected gs moveHorizontally (findKing gs) (-1) -- horizontal left
newCheckDetected gs 5 = diagonalThreatDetected gs moveDiagonally (findKing gs) (1, 1) -- diagonal up right
newCheckDetected gs 6 = diagonalThreatDetected gs moveDiagonally (findKing gs) (-1, 1) -- diagonal up left
newCheckDetected gs 7 = diagonalThreatDetected gs moveDiagonally (findKing gs) (1, -1) -- diagonal down right
newCheckDetected gs 8 = diagonalThreatDetected gs moveDiagonally (findKing gs) (-1, -1) -- diagonal down left
newCheckDetected gs _ = False -- invalid

decipherPositionDifference :: Cell -> Cell -> (Int, Int)
decipherPositionDifference (c1, r1) (c2, r2) = (fromEnum c2 - fromEnum c1, r2 - r1)

decipherDirectionDifference :: (Int, Int) -> Int
decipherDirectionDifference (0, n) | n > 0 = 1 -- vertical up
                                   | n < 0 = 2 -- vertical down
decipherDirectionDifference (n, 0) | n > 0 = 3 -- horizontal right
                                   | n < 0 = 4 -- horizontal left
decipherDirectionDifference (n, m) | n > 0 && m > 0 = 5 -- diagonal up right
                                   | n < 0 && m > 0 = 6 -- diagonal up left
                                   | n > 0 && m < 0 = 7 -- diagonal down right
                                   | n < 0 && m < 0 = 8 -- diagonal down left
decipherDirectionDifference _ = 0 -- invalid

moveVertically :: Cell -> Int -> Cell
moveVertically (c, r) n = (c, r + n)

moveHorizontally :: Cell -> Int -> Cell
moveHorizontally (c, r) n = (changingChar c n, r)

moveDiagonally :: Cell -> (Int, Int) -> Cell
moveDiagonally (c, r) (n, m) = (changingChar c n, r + m)

straightThreatDetected :: GameState -> (Cell -> Int -> Cell) -> Cell -> Int -> Bool
straightThreatDetected gs moveFunc cellCheck n | not (validCell cellCheck) = False
                                               | otherwise = case storePiece gs (moveFunc cellCheck n) of
                                                                Right _ -> straightThreatDetected gs moveFunc (moveFunc cellCheck n) n
                                                                Left p -> if belongsToCurrentPlayer gs p
                                                                            then False
                                                                            else case p of
                                                                                Rook _ _ _ -> True
                                                                                Queen _ _ _ -> True
                                                                                King _ _ _ -> straightThreatDetected gs moveFunc (moveFunc cellCheck n) n
                                                                                _ -> False

diagonalThreatDetected :: GameState -> (Cell -> (Int, Int) -> Cell) -> Cell -> (Int, Int) -> Bool
diagonalThreatDetected gs moveFunc cellCheck (n, m) | not (validCell cellCheck) = False
                                                    | otherwise = case storePiece gs (moveFunc cellCheck (n, m)) of
                                                                Right _ -> diagonalThreatDetected gs moveFunc (moveFunc cellCheck (n, m)) (n, m)
                                                                Left p -> if belongsToCurrentPlayer gs p
                                                                            then False
                                                                            else case p of
                                                                                Bishop _ _ _ -> True
                                                                                Queen _ _ _ -> True
                                                                                King _ _ _ -> diagonalThreatDetected gs moveFunc (moveFunc cellCheck (n, m)) (n, m)
                                                                                _ -> False

knightThreatDetected :: GameState -> Cell -> Bool
knightThreatDetected gs cellCheck = any (isThreateningKnight gs) possibleKnightPositions
    where
        possibleKnightPositions = [(changingChar c 1, r + 2),
                                   (changingChar c 2, r + 1),
                                   (changingChar c 2, r - 1),
                                   (changingChar c 1, r - 2),
                                   (changingChar c (-1), r - 2),
                                   (changingChar c (-2), r - 1),
                                   (changingChar c (-2), r + 1),
                                   (changingChar c (-1), r + 2)]
        (c, r) = cellCheck

isThreateningKnight :: GameState -> Cell -> Bool
isThreateningKnight gs pos = case storePiece gs pos of
                            Right _ -> False
                            Left p -> if belongsToCurrentPlayer gs p
                                        then False
                                        else case p of
                                            Knight _ _ _ -> True
                                            _ -> False

checkingCheck :: GameState -> Bool
checkingCheck gs = knightThreatDetected gs kingCell || newCheckDetected gs 1 || newCheckDetected gs 2 || newCheckDetected gs 3 || newCheckDetected gs 4 || newCheckDetected gs 5 || newCheckDetected gs 6 || newCheckDetected gs 7 || newCheckDetected gs 8
    where
        kingCell = findKing gs

-- In the process of changing vald moves to not allow for discovered check

-- king movement / castling
-- en passant (attack)
-- pawn promotion
-- checkmate
-- stalmate 
-- gameplay loop / user input