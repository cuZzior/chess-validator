package cuzz.chessvalidator.pieces

import cuzz.chessvalidator.Color.Color

case class Knight(color: Color) extends Piece {
  def rep: String = "N"
}
