import Test.HUnit
import qualified Data.Set as Set
import qualified Data.List as List

test1 = TestCase (assertChessSolution "0x0 should give 1 empty solution" [[]] (solve 0 0 []))
test2 = TestCase (assertChessSolution "1x1 should give 1 solutions" [[(Queen, (1, 1))]] (solve 1 1 [(Queen, 1)]))

test3 = TestCase (assertChessSolution "4x4 with 2 Queens should give 2 solutions" [
                                                                           [(Queen, (1, 2)), (Queen, (2, 4)), (Queen, (3, 1)), (Queen, (4, 3))]
                                                                          ,[(Queen, (1, 3)), (Queen, (2, 1)), (Queen, (3, 4)), (Queen, (4, 2))]
                                                                          ]
                                                                          (solve 4 4 [(Queen, 4)]))

data PieceType = Queen deriving (Eq, Ord, Show)

tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ]

main = runTestTT tests

genSpace :: Int -> Int -> [(Int, Int)]
genSpace x y = [(a, b) | a <- [1..x], b <- [1..y]]

type Solution = [(PieceType, (Int, Int))]

collide (x, y) fieldList = any (\(Queen, (f1, f2)) -> (f1 == x) || (f2 == y) || abs(f1 - x) == abs(f2 - y)) fieldList


sol :: [(Int, Int)] -> [PieceType]-> Solution -> [Solution]
sol _ [] found = [found]
sol [] _ found = []
sol (firstField : spaceLeft) allPieces@(nextPiece : piecesLeft) found
        | collide firstField found = sol spaceLeft allPieces found
        | otherwise = sol spaceLeft piecesLeft ((nextPiece, firstField) : found) ++ sol spaceLeft allPieces found

solve :: Int -> Int -> [(PieceType, Int)] -> [Solution]
solve x y pieces = sol (genSpace x y) (producePieces pieces) []

assertChessSolution label expected actual = assertEqual label (toSet expected) (toSet actual)
        where toSet = Set.fromList . List.map Set.fromList

producePieces :: [(PieceType, Int)] -> [PieceType]
producePieces [] = []
producePieces ((pieceType, amount) : rest) = replicate amount pieceType
