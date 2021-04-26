package cuzz.chessvalidator.pieces

import cuzz.chessvalidator.Color.Color
import cuzz.chessvalidator.Direction.Direction

case class Pawn(color: Color, movingDirection: Direction, hasMoved: Boolean = false) extends Piece {
  def rep: String = "P"
}
