module Main where

import PieceType
import ChessPuzzle

main :: IO ()
main = print . length $
  solve 7 7 [(King, 2), (Queen, 2), (Bishop, 2), (Knight, 1)]
