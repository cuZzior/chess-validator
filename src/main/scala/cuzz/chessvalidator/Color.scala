package cuzz.chessvalidator

object Color extends Enumeration {

  type Color = Value

  val WHITE: Color.Value = Value("white")
  val BLACK: Color.Value = Value("black")

  def opponentColor(color: Color): Color = {
    color match {
      case WHITE => Color.BLACK
      case BLACK => Color.WHITE
    }
  }
}
