package cuzz.chessvalidator

case class Move(source: Coordinates, destination: Coordinates) {
  override def toString: String = {
    s"from $source to $destination"
  }
}

object Move {
  def apply(sourceColumn: Int, sourceRow: Int, destinationColumn: Int, destinationRow: Int): Move =
    new Move(Coordinates(sourceColumn, sourceRow), Coordinates(destinationColumn, destinationRow))

  def apply(array: Array[Int]): Move = array match {
    case Array(srcCol, srcRow, destCol, destRow) => new Move(Coordinates(srcCol, srcRow), Coordinates(destCol, destRow))
  }
}
