package cuzz.chessvalidator.fields

import cuzz.chessvalidator.Coordinates
import cuzz.chessvalidator.pieces.Piece

trait Field {
  def coordinates: Coordinates

  def piece: Option[Piece]

  def hasPiece: Boolean

  def addPiece(piece: Piece): Field

  def removePiece(): Field
}
