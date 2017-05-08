module PieceTypeTests (pieceTypeTests) where
import Test.HUnit
import PieceType
import Data.Composition

defaultTestingPosition :: Point
defaultTestingPosition = (4, 4)

getPiece :: PieceType -> (PieceType, Point)
getPiece pieceType = (pieceType, defaultTestingPosition)

assertAllCollide :: PieceType -> [Point] -> Bool
assertAllCollide = all . pieceCollide . getPiece

--having some fun without points
assertNoneCollide :: PieceType -> [Point] -> Bool
assertNoneCollide = not .: any . pieceCollide . getPiece

rookColliding :: [Point]
rookColliding =
  [(3, 4), (2, 4), (1, 4), (5, 4), (6, 4), (4, 3), (4, 2), (4, 1), (4, 5),
   (4, 6), (4, 7)]

rookNotColliding :: [Point]
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

queenColliding :: [Point]
queenColliding = [(3, 4), (2, 4), (1, 4), (5, 4), (6, 4), (4, 3), (4, 2), (4, 1)
                 ,(4, 5), (4, 6), (4, 7), (3, 3), (2, 2), (1, 1), (5, 5), (6, 6)
                 ,(5, 3), (6, 2), (7, 1), (3, 5), (2, 6), (1, 7)
                 ]

queenNotColliding :: [Point]
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

kingColliding :: [Point]
kingColliding = [(3, 3), (3, 4), (3, 5), (4, 5), (5, 5), (5, 4), (5, 3), (4, 3)]

kingNotColliding :: [Point]
kingNotColliding = [(2, 2), (1, 4), (7, 8), (6, 5)]


kingTestNotColliding :: Test
kingTestNotColliding =
  TestCase $ assertBool
    "pieceCollide for King should return False for not colliding fields" $
    assertNoneCollide King kingNotColliding

kingTestColliding :: Test
kingTestColliding =
  TestCase $ assertBool
    "pieceCollide for King should return True for colliding fields" $
    assertAllCollide King kingColliding

bishopColliding :: [Point]
bishopColliding = [(1, 1), (2, 2), (3, 3), (5, 5), (6, 6), (5, 3), (6, 2), (3, 5)]

bishopNotColliding :: [Point]
bishopNotColliding = [(3, 4), (5, 4), (4, 5), (4, 3), (4, 1)]

bishopTestNotColliding :: Test
bishopTestNotColliding =
  TestCase $ assertBool
    "pieceCollide for Bishop should return False for not colliding fields" $
    assertNoneCollide Bishop bishopNotColliding

bishopTestColliding :: Test
bishopTestColliding =
  TestCase $ assertBool
    "pieceCollide for Bishop should return True for colliding fields" $
    assertAllCollide Bishop bishopColliding

knightColliding :: [Point]
knightColliding = [(2, 3), (2, 5), (3, 2), (3, 6), (5, 2), (5, 6), (6, 3), (6, 5)]

knightNotColliding :: [Point]
knightNotColliding = [(2, 4), (4, 2), (4, 3)]

knightTestNotColliding :: Test
knightTestNotColliding =
  TestCase $ assertBool
    "pieceCollide for Knight should return False for not colliding fields" $
    assertNoneCollide Knight knightNotColliding

knightTestColliding :: Test
knightTestColliding =
  TestCase $ assertBool
    "pieceCollide for Knight should return True for colliding fields" $
    assertAllCollide Knight knightColliding

pieceTypeTests :: Test
pieceTypeTests =  TestList 
    [ rookTestColliding 
    , rookTestNotColliding
    , queenTestColliding
    , queenTestNotColliding
    , kingTestNotColliding
    , kingTestColliding
    , bishopTestNotColliding
    , bishopTestColliding
    , knightTestNotColliding
    , knightTestColliding
    ]
