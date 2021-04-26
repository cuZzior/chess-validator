package cuzz.chessvalidator.pieces

import cuzz.chessvalidator.Color
import cuzz.chessvalidator.Color.Color


trait Piece {
  def color: Color
  def rep: String

  override def toString: String =
    color match {
      case Color.WHITE => s"${rep.toUpperCase} "
      case Color.BLACK => s"${rep.toLowerCase} "
    }
}
