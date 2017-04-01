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


test4 = TestCase (assertChessSolution "2x2 with 2 rooks should give 2 solutions" [
                [(Rook, (1, 1)), (Rook, (2, 2))],
                [(Rook, (1, 2)), (Rook, (2, 1))]
        ]
        (solve 2 2 [(Rook, 2)]))

rookCollided :: [(Int, Int)]
rookCollided = [(3, 4), (2, 4), (1, 4), (5, 4), (6, 4), (4, 3), (4, 2), (4, 1), (4, 5), (4, 6), (4, 7)]
rookNotCollided :: [(Int, Int)]
rookNotCollided = [(3, 3), (3, 2), (5, 5), (5, 6)]
defaultRookPiece = (Rook, (4, 4))


rookTest = TestCase (assertBool "Rook test" (all (\y -> pieceCollide defaultRookPiece y) rookCollided) )
rookTest2 = TestCase (assertBool "Rook test" (not (any (\y -> pieceCollide defaultRookPiece y) rookNotCollided)) )


data PieceType = Queen | Rook deriving (Eq, Ord, Show)

pieceCollide :: (PieceType, (Int, Int)) -> (Int, Int) -> Bool
pieceCollide (Queen, (x, y)) (a, b) = (x == a) || (y == b) || abs(x - a) == abs(y - b)
pieceCollide (Rook, (x, y)) (a, b) = (x == a) || (y == b)
tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ,TestLabel "test4" test4
                 ,TestLabel "rookTest" rookTest
                 ,TestLabel "rookTest2" rookTest2
                 ]

main = runTestTT tests

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

assertChessSolution label expected actual = assertEqual label (toSet expected) (toSet actual)
        where toSet = Set.fromList . List.map Set.fromList

producePieces :: [(PieceType, Int)] -> [PieceType] -- could get some unconstrained parametric polymorphism instead of PieceType, but there is no need...
producePieces [] = []
producePieces ((pieceType, amount) : rest) = replicate amount pieceType


