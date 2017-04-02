module PieceType (PieceType(..), pieceCollide) where

data PieceType = Queen | Rook deriving (Eq, Ord, Show)
pieceCollide :: (PieceType, (Int, Int)) -> (Int, Int) -> Bool
pieceCollide (Queen, (x, y)) (a, b) = (x == a) || (y == b) || abs(x - a) == abs(y - b)
pieceCollide (Rook, (x, y)) (a, b) = (x == a) || (y == b)
