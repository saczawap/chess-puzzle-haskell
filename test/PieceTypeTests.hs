module PieceTypeTests (pieceTypeTests) where
import Test.HUnit
import PieceType
import qualified Data.List as List

rookCollided :: [(Int, Int)]
rookCollided = [(3, 4), (2, 4), (1, 4), (5, 4), (6, 4), (4, 3), (4, 2), (4, 1), (4, 5), (4, 6), (4, 7)]
rookNotCollided :: [(Int, Int)]
rookNotCollided = [(3, 3), (3, 2), (5, 5), (5, 6)]
defaultRookPiece = (PieceType.Rook, (4, 4))


rookTestColliding :: Test
rookTestColliding =
  TestCase (assertBool
    "pieceCollide should return True for colliding fields"
    (all (\y -> pieceCollide defaultRookPiece y) rookCollided)
  )

rookTestNotColliding :: Test
rookTestNotColliding =
  TestCase (assertBool
  "pieceCollide should return False for not colliding fields"
  (not (any (\y -> pieceCollide defaultRookPiece y) rookNotCollided)))

failing =
  TestCase (assertBool
  "failing test"
  False)

pieceTypeTests :: Test
pieceTypeTests =  TestList [rookTestColliding, rookTestNotColliding]
