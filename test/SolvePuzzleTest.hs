module SolvePuzzleTest (solvePuzzleTests) where
import Test.HUnit
import PieceType
import ChessPuzzle
import qualified Data.Set as Set
import qualified Data.List as List

test1 :: Test
test1 = TestCase $
  assertChessSolution "0x0 should give 1 empty solution" [[]] $ solve 0 0 []

test2 :: Test
test2 = TestCase $
  assertChessSolution "1x1 should give 1 solutions" [[(Queen, (1, 1))]] $
    solve 1 1 [(Queen, 1)]

test3 :: Test
test3 = TestCase $
  assertChessSolution "4x4 with 4 Queens should give 2 solutions"
    [[(Queen, (1, 2)), (Queen, (2, 4)), (Queen, (3, 1)), (Queen, (4, 3))]
    ,[(Queen, (1, 3)), (Queen, (2, 1)), (Queen, (3, 4)), (Queen, (4, 2))]
    ] $
    solve 4 4 [(Queen, 4)]

test4 :: Test
test4 = TestCase $
  assertChessSolution "2x2 with 2 rooks should give 2 solutions"
  [[(Rook, (1, 1)), (Rook, (2, 2))]
  ,[(Rook, (1, 2)), (Rook, (2, 1))]
  ] $
  solve 2 2 [(Rook, 2)]

test5 :: Test
test5 = TestCase $
  assertChessSolution "2x3 with 1 rook and 1 Queen should give 4 solutions"
  [[(Rook, (1, 1)), (Queen, (2, 3))]
  ,[(Rook, (1, 3)), (Queen, (2, 1))]
  ,[(Rook, (2, 3)), (Queen, (1, 1))]
  ,[(Rook, (2, 1)), (Queen, (1, 3))]
  ] $
  solve 2 3 [(Rook, 1), (Queen, 1)]

test6 :: Test
test6 = TestCase $
  assertChessSolution "3x3 with 2 Kings 1 Rook"
  [[(King, (1, 1)), (King, (1, 3)), (Rook, (3, 2))]
  ,[(King, (1, 1)), (King, (3, 1)), (Rook, (2, 3))]
  ,[(King, (1, 3)), (King, (3, 3)), (Rook, (2, 1))]
  ,[(King, (3, 1)), (King, (3, 3)), (Rook, (1, 2))]
  ] $
  solve 3 3 [(King, 2), (Rook, 1)]

test7 :: Test
test7 = TestCase $
  assertChessSolution "4x4 2 rooks 4 knights"
  [[(Rook, (2, 3)), (Rook, (4, 1)), (Knight, (1, 2)), (Knight, (1, 4)), (Knight, (3, 2)), (Knight, (3, 4))]
  ,[(Rook, (2, 1)), (Rook, (4, 3)), (Knight, (1, 2)), (Knight, (1, 4)), (Knight, (3, 2)), (Knight, (3, 4))]
  ,[(Rook, (1, 1)), (Rook, (3, 3)), (Knight, (2, 2)), (Knight, (2, 4)), (Knight, (4, 2)), (Knight, (4, 4))]
  ,[(Rook, (1, 3)), (Rook, (3, 1)), (Knight, (2, 2)), (Knight, (2, 4)), (Knight, (4, 2)), (Knight, (4, 4))]
  ,[(Rook, (1, 2)), (Rook, (3, 4)), (Knight, (2, 1)), (Knight, (2, 3)), (Knight, (4, 1)), (Knight, (4, 3))]
  ,[(Rook, (1, 4)), (Rook, (3, 2)), (Knight, (2, 1)), (Knight, (2, 3)), (Knight, (4, 1)), (Knight, (4, 3))]
  ,[(Rook, (2, 4)), (Rook, (4, 2)), (Knight, (1, 1)), (Knight, (1, 3)), (Knight, (3, 1)), (Knight, (3, 3))]
  ,[(Rook, (2, 2)), (Rook, (4, 4)), (Knight, (1, 1)), (Knight, (1, 3)), (Knight, (3, 1)), (Knight, (3, 3))]

  ] $
  solve 4 4 [(Knight, 4), (Rook, 2)]


solvePuzzleTests :: Test
solvePuzzleTests =
  TestList [test1
           ,test2
           ,test3
           ,test4
           ,test5
           ,test6
           ,test7
           ]

assertChessSolution :: String -> [Solution] -> [Solution] -> Assertion
assertChessSolution label expected actual =
    assertEqual label (toSet expected) (toSet actual)
  where toSet = Set.fromList . List.map Set.fromList
