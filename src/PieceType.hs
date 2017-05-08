module PieceType (PieceType(..), pieceCollide) where

import Data.List

data PieceType = Queen | Rook | King | Bishop | Knight deriving (Eq, Ord, Show)
pieceCollide :: (PieceType, (Int, Int)) -> (Int, Int) -> Bool
pieceCollide (Queen, (x, y)) (a, b) = (x == a) || (y == b) || abs(x - a) == abs(y - b)
pieceCollide (Rook, (x, y)) (a, b) = (x == a) || (y == b)
pieceCollide (King, (x, y)) (a, b) = (abs (x - a) <= 1) && (abs (y - b) <= 1)
pieceCollide (Bishop, (x, y)) (a, b) = abs(x - a) == abs(y - b)
pieceCollide (Knight, (x, y)) (a, b) = ((abs (x - a) == 1) && (abs (y - b) == 2)) || ((abs (x - a) == 2) && (abs (y - b) == 1))
