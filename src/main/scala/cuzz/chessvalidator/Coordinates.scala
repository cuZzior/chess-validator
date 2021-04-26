package cuzz.chessvalidator

case class Coordinates(column: Int, row: Int) {
  override def toString: String = s"[$column, $row]"
}
