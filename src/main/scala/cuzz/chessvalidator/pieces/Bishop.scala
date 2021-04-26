package cuzz.chessvalidator.pieces

import cuzz.chessvalidator.Color.Color

case class Bishop(color: Color) extends Piece {
  def rep: String = "B"
}
