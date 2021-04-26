package cuzz.chessvalidator.fields

import cuzz.chessvalidator.Coordinates
import cuzz.chessvalidator.pieces.Piece

case class Square(coordinates: Coordinates, piece: Option[Piece]) extends Field {
  def hasPiece: Boolean = piece.isDefined
  def addPiece(newPiece: Piece): Square = this.copy(piece = Some(newPiece))
  def removePiece(): Square = this.copy(piece = None)

  override def toString: String = if (!hasPiece) "--" else piece.get.toString
}

object Square {
  def apply(column: Int, row: Int): Square = new Square(Coordinates(column, row), None)
  def apply(column: Int, row: Int, piece: Piece): Square = new Square(Coordinates(column, row), Some(piece))
  def apply(column: Int, row: Int, piece: Option[Piece]): Square = new Square(Coordinates(column, row), piece)
}

final case class SquareException(message: String) extends Exception(message)
