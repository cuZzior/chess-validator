package cuzz.chessvalidator

import cuzz.chessvalidator.boards.ClassicBoard
import cuzz.chessvalidator.fields.Square
import cuzz.chessvalidator.pieces.Pawn
import org.scalatest.{FlatSpec, Matchers}

class ClassicBoardTest extends FlatSpec with Matchers{

  "ClassicBoard.modifySquare" should "replace square with new one" in {
    val newSquare = Square(0, 2, Pawn(Color.BLACK, Direction.BOTTOM))
    val board = ClassicBoard(Seq(Square(0, 0), Square(0, 1), Square(0, 2), Square(0, 3)))

    val newBoardState = board.modify(newSquare)
    newBoardState.fields.size shouldBe 4
    newBoardState.fields.contains(newSquare) shouldBe true
  }

  "ClassicBoard.getFieldBy" should "return field from board" in {
    val squareWithPiece = Square(0, 2, Pawn(Color.BLACK, Direction.BOTTOM))
    val board = ClassicBoard(Seq(Square(0, 0), Square(0, 1), squareWithPiece, Square(0, 3)))
    board.getFieldBy(squareWithPiece.coordinates) shouldBe Some(squareWithPiece)
    board.getFieldBy(squareWithPiece.coordinates).get.hasPiece shouldBe true
  }
}
