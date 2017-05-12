module ChessPuzzle(solve, Solution, producePieces) where

import Test.HUnit
import PieceType
import qualified Data.Set as Set
import qualified Data.List as List

genSpace :: Int -> Int -> [(Int, Int)]
genSpace x y = [(a, b) | a <- [1..x], b <- [1..y]]

type Solution = [(PieceType, (Int, Int))]

collide :: (PieceType, (Int, Int)) -> [(PieceType, (Int, Int))] -> Bool
collide piece@(newPt, newCoord) = any (\(_pt, field) -> pieceCollide piece field)


sol :: [(Int, Int)] -> [PieceType]-> Solution -> [Solution]
sol _ [] found = [found]
sol [] _ _found = []
sol (firstField : spaceLeft) allPieces@(nextPiece : piecesLeft) found
        | collide (nextPiece, firstField) found = otherSolutions
        | otherwise = sol (trimSpace spaceLeft (nextPiece, firstField)) piecesLeft ((nextPiece, firstField) : found) ++ otherSolutions
        where otherSolutions = sol spaceLeft allPieces found

trimSpace :: [Point] -> (PieceType, Point)  -> [Point]
trimSpace a b = filter (not . pieceCollide b) a

solve :: Int -> Int -> [(PieceType, Int)] -> [Solution]
solve x y pieces = List.concatMap (\z -> sol (genSpace x y) z []) (producePieces pieces)

producePieces :: [(PieceType, Int)] -> [[PieceType]]
producePieces [] = [[]]
producePieces toPlace = List.nub $ List.permutations $ concatMap (\(x, y) -> replicate y x) toPlace
