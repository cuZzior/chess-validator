package cuzz.chessvalidator

import cuzz.chessvalidator.fields.Square
import cuzz.chessvalidator.pieces.{King, Piece}
import org.scalatest.{FlatSpec, Matchers}

class SquareTest extends FlatSpec with Matchers {
  trait TestContext {
    val piece: Piece = King(Color.WHITE)
    val emptySquare: Square = Square(0, 0)
    val takenSquare: Square = Square(0, 0, piece)
  }

  "Square.hasPiece" should "return false for empty square" in new TestContext {
    emptySquare.hasPiece shouldBe false
  }

  "Square.hasPiece" should "return true for taken square" in new TestContext {
    takenSquare.hasPiece shouldBe true
  }
}
