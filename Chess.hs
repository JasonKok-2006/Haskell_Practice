-- Welcome to my attempt at coding chess within haskell.

{-

 ___ ___ ___ ___ ___ ___ ___ ___ 
|   |   |   |   | ^ |   |   |   |
| r | k | b | q | k | b | k | r |
|___|___|___|___|___|___|___|___|
|   |   |   |   |   |   |   |   |
| p | p | p | p | p | p | p | p |
|___|___|___|___|___|___|___|___|
|   |   |   |   |   |   |   |   |
|   |   |   |   |   |   |   |   |
|___|___|___|___|___|___|___|___|
|   |   |   |   |   |   |   |   |
|   |   |   |   |   |   |   |   |
|___|___|___|___|___|___|___|___|
|   |   |   |   |   |   |   |   |
|   |   |   |   |   |   |   |   |
|___|___|___|___|___|___|___|___|
|   |   |   |   |   |   |   |   |
|   |   |   |   |   |   |   |   |
|___|___|___|___|___|___|___|___|
|   |   |   |   |   |   |   |   |
| P | P | P | P | P | P | P | P |
|___|___|___|___|___|___|___|___|
|   |   |   |   | ^ |   |   |   |
| R | K | B | Q | K | B | K | R |
|___|___|___|___|___|___|___|___|

-}

-- Warning ignorations set throughout the project.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

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
    deriving Show

-- Players Code
data Players = White | Black 
    deriving Show

type Pieces = [Piece Players Moves Cell]
type GameState = [(Cell, Maybe (Piece Players Moves Cell))]

selectPlayer :: Int -> Players
selectPlayer n = if even n then White else Black

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
createGameState :: GameBoard -> Pieces -> GameState
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