package cuzz.chessvalidator

object Direction extends Enumeration {

  type Direction = Value

  val TOP: Direction.Value = Value(0)
  val TOP_RIGHT: Direction.Value = Value(1)
  val RIGHT: Direction.Value = Value(2)
  val BOTTOM_RIGHT: Direction.Value = Value(3)
  val BOTTOM: Direction.Value = Value(4)
  val BOTTOM_LEFT: Direction.Value = Value(5)
  val LEFT: Direction.Value = Value(6)
  val TOP_LEFT: Direction.Value = Value(7)

  def defineMoveDirection(move: Move): Direction.Value = {
    if (
      move.destination.column > move.source.column
      && move.destination.row == move.source.row
    ) Direction.RIGHT

    else if (
      move.destination.column < move.source.column
      && move.destination.row == move.source.row
    ) Direction.LEFT

    else if (
      move.destination.column == move.source.column
      && move.destination.row > move.source.row
    ) Direction.BOTTOM

    else if (
      move.destination.column == move.source.column
      && move.destination.row < move.source.row
    ) Direction.TOP

    else if (
      move.destination.column > move.source.column
      && move.destination.row > move.source.row
      && isDiagonal(move)
    ) Direction.BOTTOM_RIGHT

    else if (
      move.destination.column < move.source.column
      && move.destination.row > move.source.row
      && isDiagonal(move)
    ) Direction.BOTTOM_LEFT

    else if (
      move.destination.column < move.source.column
      && move.destination.row < move.source.row
      && isDiagonal(move)
    ) Direction.TOP_LEFT

    else if (
      move.destination.column > move.source.column
      && move.destination.row < move.source.row
      && isDiagonal(move)
    ) Direction.TOP_RIGHT

    else throw new MoveException("Invalid move direction. Has to move vertical, horizontal or diagonal.")
  }

  private def isDiagonal(move: Move): Boolean = {
    (move.source.column - move.destination.column).abs == (move.source.row - move.destination.row).abs
  }
}
