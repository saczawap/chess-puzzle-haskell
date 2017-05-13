module PieceType (PieceType(..), pieceCollide, Point) where

import Data.List
type Point = (Int, Int)

data PieceType = Queen | Rook | King | Bishop | Knight deriving (Eq, Ord, Show)

pieceCollide :: (PieceType, (Int, Int)) -> (Int, Int) -> Bool
pieceCollide (Queen, p1) p2 = onStraightLine p1 p2 || onDiagonal p1 p2
pieceCollide (Rook, p1) p2 = onStraightLine p1 p2
pieceCollide (King, (x1, y1)) (x2, y2) =
  (abs (x1 - x2) <= 1) && (abs (y1 - y2) <= 1)
pieceCollide (Bishop, p1) p2 = onDiagonal p1 p2
pieceCollide (Knight, (x1, y1)) (x2, y2) =
  ((abs (x1 - x2) == 1) && (abs (y1 - y2) == 2)) ||
  ((abs (x1 - x2) == 2) && (abs (y1 - y2) == 1))

onStraightLine :: Point -> Point -> Bool
onStraightLine (x1, y1) (x2, y2) = (x1 == x2) || (y1 == y2)

onDiagonal :: Point -> Point -> Bool
onDiagonal (x1, y1) (x2, y2) = abs(x1 - x2) == abs(y1 - y2)
