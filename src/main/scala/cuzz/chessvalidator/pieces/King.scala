package cuzz.chessvalidator.pieces

import cuzz.chessvalidator.Color.Color

case class King(color: Color) extends Piece {
  def rep: String = "K"
}
