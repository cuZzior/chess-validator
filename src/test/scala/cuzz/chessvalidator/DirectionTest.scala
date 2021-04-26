package cuzz.chessvalidator

import org.scalatest.{FlatSpec, Matchers}

class DirectionTest extends FlatSpec with Matchers {

  "Direction" should "return correct move direction" in {
    val rightMove: Move = Move(2, 7, 4, 7)
    Direction.defineMoveDirection(rightMove) shouldBe Direction.RIGHT

    val leftMove: Move = Move(2, 7, 1, 7)
    Direction.defineMoveDirection(leftMove) shouldBe Direction.LEFT

    val topMove: Move = Move(2, 7, 2, 6)
    Direction.defineMoveDirection(topMove) shouldBe Direction.TOP

    val bottomMove: Move = Move(2, 5, 2, 6)
    Direction.defineMoveDirection(bottomMove) shouldBe Direction.BOTTOM

    val topRightMove: Move = Move(1, 7, 3, 5)
    Direction.defineMoveDirection(topRightMove) shouldBe Direction.TOP_RIGHT

    val topLeftMove: Move = Move(5, 6, 3, 4)
    Direction.defineMoveDirection(topLeftMove) shouldBe Direction.TOP_LEFT

    val bottomRightMove: Move = Move(3, 4, 5, 6)
    Direction.defineMoveDirection(bottomRightMove) shouldBe Direction.BOTTOM_RIGHT

    val bottomLeftMove: Move = Move(6, 4, 4, 6)
    Direction.defineMoveDirection(bottomLeftMove) shouldBe Direction.BOTTOM_LEFT
  }

  "Direction" should "throw exception for not-straight move eg. like knight" in {
    val knightMove: Move = Move(1, 7, 2, 5)
    a[MoveException] shouldBe thrownBy(Direction.defineMoveDirection(knightMove))
  }
}
