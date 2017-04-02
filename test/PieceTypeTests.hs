module PieceTypeTests (pieceTypeTests) where
import Test.HUnit
import PieceType
import Data.Composition

defaultTestingPosition :: (Int, Int)
defaultTestingPosition = (4, 4)

getPiece :: PieceType -> (PieceType, (Int, Int))
getPiece pieceType = (pieceType, defaultTestingPosition)

assertAllCollide :: PieceType -> [(Int, Int)] -> Bool
assertAllCollide = all . pieceCollide . getPiece

--having some fun without points
assertNoneCollide :: PieceType -> [(Int, Int)] -> Bool
assertNoneCollide = not .: any . pieceCollide . getPiece

rookColliding :: [(Int, Int)]
rookColliding =
  [(3, 4), (2, 4), (1, 4), (5, 4), (6, 4), (4, 3), (4, 2), (4, 1), (4, 5),
   (4, 6), (4, 7)]

rookNotColliding :: [(Int, Int)]
rookNotColliding = [(3, 3), (3, 2), (5, 5), (5, 6)]

rookTestColliding :: Test
rookTestColliding =
  TestCase $ assertBool
    "pieceCollide for Rook should return True for colliding fields" $
    assertAllCollide Rook rookColliding


rookTestNotColliding :: Test
rookTestNotColliding =
  TestCase $ assertBool
    "pieceCollide for Rook should return False for not colliding fields" $
    assertNoneCollide Rook rookNotColliding

queenColliding :: [(Int, Int)]
queenColliding = [(3, 4), (2, 4), (1, 4), (5, 4), (6, 4), (4, 3), (4, 2), (4, 1)
                 ,(4, 5), (4, 6), (4, 7), (3, 3), (2, 2), (1, 1), (5, 5), (6, 6)
                 ,(5, 3), (6, 2), (7, 1), (3, 5), (2, 6), (1, 7)
                 ]

queenNotColliding :: [(Int, Int)]
queenNotColliding = [(2, 3), (2, 1), (6, 5), (7, 6), (7, 5), (2, 5)]

queenTestColliding :: Test
queenTestColliding =
  TestCase $ assertBool
    "pieceCollide for Queen should return True for colliding fields" $
    assertAllCollide Queen queenColliding


queenTestNotColliding :: Test
queenTestNotColliding =
  TestCase $ assertBool
    "pieceCollide for Queen should return False for not colliding fields" $
    assertNoneCollide Queen queenNotColliding


pieceTypeTests :: Test
pieceTypeTests =  TestList [rookTestColliding
                           ,rookTestNotColliding
                           ,queenTestColliding
                           ,queenTestNotColliding
                           ]
