import Test.HUnit
import qualified Data.Set as Set
import qualified Data.List as List

test1 = TestCase (assertChessSolution "0x0 should give 1 empty solution" [[]] (solve 0 0))
test2 = TestCase (assertChessSolution "1x1 should give 1 solutions" [[(1, 1)]] (solve 1 1))

test3 = TestCase (assertChessSolution "4x4 with 2 Queens should give 2 solutions" [[(1, 2), (2, 4), (3, 1), (4, 3)]
                                                                          ,[(1, 3), (2, 1), (3, 4), (4, 2)]
                                                                          ]
                                                                          (solve 4 4))


tests = TestList [TestLabel "test1" test1
                 ,TestLabel "test2" test2
                 ,TestLabel "test3" test3
                 ]

main = runTestTT tests

genSpace :: Int -> Int -> [(Int, Int)]
genSpace x y = [(a, b) | a <- [1..x], b <- [1..y]]

type Solution = [(Int, Int)]

collide (x, y) fieldList = any (\(f1, f2) -> (f1 == x) || (f2 == y) || abs(f1 - x) == abs(f2 - y)) fieldList


sol _ 0 found = [found]
sol [] _ found = []
sol (firstField : spaceLeft) piecesLeft found
        | collide firstField found = sol spaceLeft piecesLeft found
        | otherwise = sol spaceLeft (piecesLeft - 1) (firstField : found) ++ sol spaceLeft piecesLeft found

solve :: Int -> Int -> [Solution]
solve x y = sol (genSpace x y) x []

assertChessSolution label expected actual = assertEqual label (toSet expected) (toSet actual)
        where toSet = Set.fromList . List.map Set.fromList
