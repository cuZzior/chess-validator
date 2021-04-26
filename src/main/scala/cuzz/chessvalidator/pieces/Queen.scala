package cuzz.chessvalidator.pieces

import cuzz.chessvalidator.Color.Color

case class Queen(color: Color) extends Piece {
  def rep: String = "Q"
}
