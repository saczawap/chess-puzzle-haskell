import Test.HUnit
import PieceTypeTests
import SolvePuzzleTest

tests :: Test
tests = TestList[pieceTypeTests, solvePuzzleTests]

main :: IO (Counts)
main = runTestTT tests
