module ChessPuzzle(solve, Solution) where

import Test.HUnit
import PieceType
import qualified Data.Set as Set
import qualified Data.List as List

genSpace :: Int -> Int -> [(Int, Int)]
genSpace x y = [(a, b) | a <- [1..x], b <- [1..y]]

type Solution = [(PieceType, (Int, Int))]

collide :: (PieceType, (Int, Int)) -> [(PieceType, (Int, Int))] -> Bool
collide piece fieldList = any (\(_, field) -> pieceCollide piece field) fieldList


sol :: [(Int, Int)] -> [PieceType]-> Solution -> [Solution]
sol _ [] found = [found]
sol [] _ found = []
sol (firstField : spaceLeft) allPieces@(nextPiece : piecesLeft) found
        | collide (nextPiece, firstField) found = sol spaceLeft allPieces found
        | otherwise = sol spaceLeft piecesLeft ((nextPiece, firstField) : found) ++ sol spaceLeft allPieces found

solve :: Int -> Int -> [(PieceType, Int)] -> [Solution]
solve x y pieces = sol (genSpace x y) (producePieces pieces) []


producePieces :: [(PieceType, Int)] -> [PieceType] -- could get some unconstrained parametric polymorphism instead of PieceType, but there is no need...
producePieces [] = []
producePieces ((pieceType, amount) : rest) = replicate amount pieceType
