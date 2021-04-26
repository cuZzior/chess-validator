package cuzz.chessvalidator.pieces

import cuzz.chessvalidator.Color.Color

case class Rook(color: Color) extends Piece {
  def rep: String = "R"
}
