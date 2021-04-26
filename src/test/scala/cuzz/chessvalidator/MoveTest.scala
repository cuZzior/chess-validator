package cuzz.chessvalidator

import org.scalatest.{FlatSpec, Matchers}

class MoveTest extends FlatSpec with Matchers {
  "Move" should "be correctly represented as string" in {
    val move: Move = Move(1, 7, 2, 5)
    move.toString shouldBe "from [1, 7] to [2, 5]"
  }
}
