package cuzz.chessvalidator

import cuzz.chessvalidator.pieces.{Bishop, King, Knight, Pawn, Queen, Rook}
import org.scalatest.{FlatSpec, Matchers}

class PieceTest extends FlatSpec with Matchers {
  "Piece" should "be printed as single upper or lower case letter followed by space" in {
    Bishop(Color.WHITE).toString shouldBe "B "
    Bishop(Color.BLACK).toString shouldBe "b "
    King(Color.WHITE).toString shouldBe "K "
    King(Color.BLACK).toString shouldBe "k "
    Knight(Color.WHITE).toString shouldBe "N "
    Knight(Color.BLACK).toString shouldBe "n "
    Pawn(Color.WHITE, Direction.TOP).toString shouldBe "P "
    Pawn(Color.BLACK, Direction.TOP).toString shouldBe "p "
    Queen(Color.WHITE).toString shouldBe "Q "
    Queen(Color.BLACK).toString shouldBe "q "
    Rook(Color.WHITE).toString shouldBe "R "
    Rook(Color.BLACK).toString shouldBe "r "
  }
}
