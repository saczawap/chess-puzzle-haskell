module ChessPuzzle(solve, Solution, producePieces) where

import Test.HUnit
import PieceType
import qualified Data.Set as Set
import qualified Data.List as List

genSpace :: Int -> Int -> [(Int, Int)]
genSpace x y = [(a, b) | a <- [1..x], b <- [1..y]]

type Solution = [(PieceType, (Int, Int))]

collide :: (PieceType, (Int, Int)) -> [(PieceType, (Int, Int))] -> Bool
collide piece@(newPt, newCoord) fieldList = any (\existing@(pt, field) -> (pieceCollide piece field) || (pieceCollide existing newCoord)) fieldList


sol :: [(Int, Int)] -> [PieceType]-> Solution -> [Solution]
sol _ [] found = [found]
sol [] _ _found = []
sol (firstField : spaceLeft) allPieces@(nextPiece : piecesLeft) found
        | collide (nextPiece, firstField) found = sol spaceLeft allPieces found
        | otherwise = sol spaceLeft piecesLeft ((nextPiece, firstField) : found) ++ sol spaceLeft allPieces found

solve :: Int -> Int -> [(PieceType, Int)] -> [Solution]
solve x y pieces = List.concatMap (\z -> sol (genSpace x y) z []) (producePieces pieces)

producePieces :: [(PieceType, Int)] -> [[PieceType]]
producePieces [] = [[]]
producePieces toPlace = List.nub $ List.permutations $ concatMap (\(x, y) -> replicate y x) toPlace
