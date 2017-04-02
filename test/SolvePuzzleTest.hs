module SolvePuzzleTest (solvePuzzleTests) where
import Test.HUnit
import PieceType
import ChessPuzzle
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

        
solvePuzzleTests :: Test
solvePuzzleTests = TestList [test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4]


assertChessSolution label expected actual = assertEqual label (toSet expected) (toSet actual)
        where toSet = Set.fromList . List.map Set.fromList
