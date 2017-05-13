module ChessPuzzle(solve, Solution, producePieces) where

import Test.HUnit
import PieceType
import qualified Data.Set as Set
import qualified Data.List as List

type PlacedPiece = (PieceType, Point)
type Solution = [PlacedPiece]
type SolutionSpace = [Point]

solve :: Int -> Int -> [(PieceType, Int)] -> [Solution]
solve x y pieces = List.concatMap (\pieces -> solve' possibleSpace pieces []) possiblePieces
  where possibleSpace = genSpace x y
        possiblePieces = producePieces pieces

solve' :: SolutionSpace -> [PieceType]-> Solution -> [Solution]
solve' _ [] found = [found]
solve' [] _ _found = []
solve' (firstField : spaceLeft) allPieces@(nextPiece : piecesLeft) found
  | collide (nextPiece, firstField) found = otherSolutions
  | otherwise = solve' trimmedSpace piecesLeft pieceAdded ++ otherSolutions
  where otherSolutions = solve' spaceLeft allPieces found
        pieceToPlace = (nextPiece, firstField)
        pieceAdded = pieceToPlace : found
        trimmedSpace = trimSpace pieceToPlace spaceLeft

genSpace :: Int -> Int -> SolutionSpace
genSpace x y = [(a, b) | a <- [1..x], b <- [1..y]]

collide :: PlacedPiece -> Solution -> Bool
collide piece = any (\(_pt, field) -> pieceCollide piece field)

trimSpace :: PlacedPiece -> SolutionSpace -> SolutionSpace
trimSpace alreadyPlaced = filter (not . pieceCollide alreadyPlaced)

producePieces :: [(PieceType, Int)] -> [[PieceType]]
producePieces [] = [[]]
producePieces toPlace =
  List.nub $ List.permutations $ concatMap (\(x, y) -> replicate y x) toPlace
  
