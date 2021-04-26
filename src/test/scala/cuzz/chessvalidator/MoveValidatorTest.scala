package cuzz.chessvalidator

import cuzz.chessvalidator.Color.Color
import cuzz.chessvalidator.boards.{Board, ClassicBoard}
import cuzz.chessvalidator.fields.{Field, Square}
import cuzz.chessvalidator.moveGenerator.{ClassicMoveGenerator, MoveGenerator}
import cuzz.chessvalidator.pieces.{Pawn, Piece, Queen}
import org.mockito.MockitoSugar.{mock, when}
import org.scalatest.{FlatSpec, Matchers}

class MoveValidatorTest extends FlatSpec with Matchers{

  trait TestContext {
    val moveGeneratorMock: MoveGenerator = mock[MoveGenerator]
    val boardMock: Board = mock[Board]
    val moveValidator = new MoveValidator(moveGeneratorMock)
    val colorMock: Color = mock[Color]
    val fieldMock: Field = mock[Field]
    val pieceMock: Piece = mock[Piece]
  }

  trait ClassicTestContext {
    val moveValidator = new MoveValidator(new ClassicMoveGenerator)
  }

  "MoveValidator" should "throw exception if source square is out of bounds" in new TestContext {
    val move: Move = Move(2, 6, 2, 5)
    when(boardMock.getFieldBy(move.source)).thenReturn(None)
    the [MoveException] thrownBy
      moveValidator.validate(move, boardMock, colorMock) should have message "Source square out of bounds."
  }

  "MoveValidator" should "throw exception if destination square is out of bounds" in new TestContext {
    val move: Move = Move(2, 6, 2, 5)
    when(boardMock.getFieldBy(move.source)).thenReturn(Some(fieldMock))
    when(boardMock.getFieldBy(move.destination)).thenReturn(None)
    the [MoveException] thrownBy
      moveValidator.validate(move, boardMock, colorMock) should have message "Destination square out of bounds."
  }

  "MoveValidator" should "throw exception if source square is empty" in new TestContext {
    val move: Move = Move(2, 6, 2, 5)
    when(boardMock.getFieldBy(move.source)).thenReturn(Some(fieldMock))
    when(boardMock.getFieldBy(move.destination)).thenReturn(Some(fieldMock))
    when(fieldMock.hasPiece).thenReturn(false)

    the [MoveException] thrownBy
      moveValidator.validate(move, boardMock, colorMock) should have message "Source square empty."
  }

  "MoveValidator" should "throw exception if destination equals source" in new TestContext {
    val move: Move = Move(2, 6, 2, 6)
    when(boardMock.getFieldBy(move.source)).thenReturn(Some(fieldMock))
    when(boardMock.getFieldBy(move.destination)).thenReturn(Some(fieldMock))
    when(fieldMock.hasPiece).thenReturn(true)

    the [MoveException] thrownBy
      moveValidator.validate(move, boardMock, colorMock) should have message "Piece has to be moved from source location."
  }

  "MoveValidator" should "throw exception if moved piece is different color then current player" in new TestContext {
    val move: Move = Move(2, 6, 2, 5)
    val whiteColor: Color = Color.WHITE
    when(boardMock.getFieldBy(move.source)).thenReturn(Some(fieldMock))
    when(boardMock.getFieldBy(move.destination)).thenReturn(Some(fieldMock))
    when(fieldMock.hasPiece).thenReturn(true)
    when(fieldMock.piece).thenReturn(Some(pieceMock))
    when(fieldMock.piece.get.color).thenReturn(whiteColor)

    the [MoveException] thrownBy
      moveValidator.validate(move, boardMock, Color.opponentColor(whiteColor)) should have message "Piece is different color than current player."
  }

  "MoveValidator" should "throw exception if destination square has current player piece" in new TestContext {
    val move: Move = Move(2, 6, 2, 5)
    val whiteColor: Color = Color.WHITE
    when(boardMock.getFieldBy(move.source)).thenReturn(Some(fieldMock))
    when(boardMock.getFieldBy(move.destination)).thenReturn(Some(fieldMock))
    when(fieldMock.hasPiece).thenReturn(true)
    when(fieldMock.piece).thenReturn(Some(pieceMock))
    when(fieldMock.piece.get.color).thenReturn(whiteColor)

    the [MoveException] thrownBy
      moveValidator.validate(move, boardMock, whiteColor) should have message "Destination square is taken by your other piece."
  }

  "MoveValidator" should "throw exception if passed piece is not supported" in new TestContext {
    val move: Move = Move(0, 7, 0, 4)
    val destFieldMock: Field = mock[Field]
    when(boardMock.getFieldBy(move.source)).thenReturn(Some(fieldMock))
    when(boardMock.getFieldBy(move.destination)).thenReturn(Some(destFieldMock))
    when(fieldMock.hasPiece).thenReturn(true)
    when(fieldMock.piece).thenReturn(Some(pieceMock))
    when(fieldMock.piece.get.color).thenReturn(colorMock)

    the [MoveException] thrownBy
      moveValidator.validate(move, boardMock, colorMock) should have message "Piece not supported."
  }

  "MoveValidator" should "throw exception if piece is in path for far-moving pieces" in new ClassicTestContext {
    val move: Move = Move(0, 7, 0, 4)

    the [MoveException] thrownBy
      moveValidator.validate(move, ClassicBoard(), Color.WHITE) should have message "Invalid move. Another piece is blocking path."
  }

  "MoveValidator" should "throw exception if pawn is making invalid move" in new ClassicTestContext {
    val move: Move = Move(1, 6, 1, 2)

    the [MoveException] thrownBy
      moveValidator.validate(move, ClassicBoard(), Color.WHITE) should have message "Invalid pawn move."
  }

  "MoveValidator" should "throw exception if pawn is making diagonal move without attack" in new ClassicTestContext {
    val move: Move = Move(1, 6, 2, 5)

    the [MoveException] thrownBy
      moveValidator.validate(move, ClassicBoard(), Color.WHITE) should have message "Cannot move diagonally without attack."
  }

  "MoveValidator" should "throw exception if pawn is moving by 2 fields on subsequent move" in new ClassicTestContext {
    val move: Move = Move(1, 6, 1, 4)
    val pawnThatMoved: Pawn = Pawn(Color.WHITE, Direction.TOP, hasMoved = true)

    the [MoveException] thrownBy
      moveValidator.validate(move, ClassicBoard().modify(Square(1, 6, pawnThatMoved)), Color.WHITE) should have message
      "Cannot move by two squares on subsequent move."
  }

  "MoveValidator" should "throw exception if knight is making invalid move" in new ClassicTestContext {
    val move: Move = Move(1, 7, 1, 6)

    the [MoveException] thrownBy
      moveValidator.validate(move, ClassicBoard().modify(Square(1, 6, None)), Color.WHITE) should have message "Invalid knight move."
  }

  "MoveValidator" should "throw exception if king is making invalid move" in new ClassicTestContext {
    val move: Move = Move(4, 7, 4, 5)

    the [MoveException] thrownBy
      moveValidator.validate(move, ClassicBoard().modify(Square(4, 6, None)), Color.WHITE) should have message "Invalid king move."
  }

  "MoveValidator" should "return true if king in check" in new ClassicTestContext {
    val boardWithKingInCheck: ClassicBoard = ClassicBoard()
      .modify(Square(4, 6, Queen(Color.WHITE)))
      .modify(Square(4, 1, None))
    moveValidator.isCurrentPlayerKingInCheck(boardWithKingInCheck, Color.BLACK) shouldBe true
  }

  "MoveValidator" should "throw exception if player end move with it's king still in check" in new ClassicTestContext {
    val boardWithKingInCheck: ClassicBoard = ClassicBoard()
      .modify(Square(4, 6, Queen(Color.WHITE)))
      .modify(Square(4, 1, None))

    the[MoveException] thrownBy
      moveValidator.validate(Move(3, 1, 3, 2), boardWithKingInCheck, Color.BLACK) should have message "black king still in check."
  }
}
