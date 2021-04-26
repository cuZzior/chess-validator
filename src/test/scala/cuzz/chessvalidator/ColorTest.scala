package cuzz.chessvalidator

import org.scalatest.{FlatSpec, Matchers}

class ColorTest extends FlatSpec with Matchers {

  "Color.opponentColor" should "return different color" in {
    Color.opponentColor(Color.WHITE) shouldBe Color.BLACK
    Color.opponentColor(Color.BLACK) shouldBe Color.WHITE
  }
}
