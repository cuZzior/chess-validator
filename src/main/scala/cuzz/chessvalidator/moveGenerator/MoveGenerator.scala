package cuzz.chessvalidator.moveGenerator

import cuzz.chessvalidator.{Coordinates, Move}
import cuzz.chessvalidator.pieces.Piece

trait MoveGenerator {

  def generate(piece: Piece, move: Move): Seq[Coordinates]
}
