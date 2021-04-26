package cuzz.chessvalidator.boards

import cuzz.chessvalidator.Coordinates
import cuzz.chessvalidator.fields.Field

trait Board {

  def fields: Seq[Field]

  def modify(field: Field): Board

  def getFieldBy(coordinates: Coordinates): Option[Field]

  def renderBoard(): Unit
}
