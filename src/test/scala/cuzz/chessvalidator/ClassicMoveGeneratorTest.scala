package cuzz.chessvalidator

import cuzz.chessvalidator.moveGenerator.ClassicMoveGenerator
import cuzz.chessvalidator.pieces._
import org.scalatest.{FlatSpec, Matchers}

class ClassicMoveGeneratorTest extends FlatSpec with Matchers{

  trait TestContext {
    val moveGenerator = new ClassicMoveGenerator
    val rook: Rook = Rook(Color.BLACK)
    val queen: Queen = Queen(Color.WHITE)
    val bishop: Bishop = Bishop(Color.WHITE)
    val king: King = King(Color.BLACK)
    val knight: Knight = Knight(Color.WHITE)
  }

  "ClassicMoveGenerator" should "throw exception for invalid far-moving piece move" in new TestContext {
    val invalidBishopMove: Move = Move(5, 7, 5, 5)
    a[MoveException] shouldBe thrownBy(moveGenerator.generate(bishop, invalidBishopMove))

    val invalidQueenMove: Move = Move(3, 7, 4, 5)
    a[MoveException] shouldBe thrownBy(moveGenerator.generate(queen, invalidQueenMove))


    val invalidRookMove: Move = Move(1, 6, 4, 3)
    a[MoveException] shouldBe thrownBy(moveGenerator.generate(rook, invalidRookMove))
  }

  "ClassicMoveGenerator" should "generate seq of coordinates between source and destination for vertical move" in new TestContext {

    val moveToTop: Move = Move(0, 7, 0, 3)
    val expectedSeq: Seq[Coordinates] = Seq(Coordinates(0, 6), Coordinates(0, 5), Coordinates(0, 4))
    moveGenerator.generate(rook, moveToTop) shouldBe expectedSeq
    moveGenerator.generate(queen, moveToTop) shouldBe expectedSeq

    val moveToBottom: Move = Move(0, 2, 0, 5)
    val expectedSeq2 = Seq(Coordinates(0, 3), Coordinates(0, 4))
    moveGenerator.generate(rook, moveToBottom) shouldBe expectedSeq2
    moveGenerator.generate(queen, moveToBottom) shouldBe expectedSeq2
  }

  "ClassicMoveGenerator" should "generate seq of coordinates between source and destination for horizontal move" in new TestContext {
    val moveToLeft: Move = Move(6, 5, 2 ,5)
    val expectedSeq: Seq[Coordinates] = Seq(Coordinates(5, 5), Coordinates(4, 5), Coordinates(3, 5))
    moveGenerator.generate(rook, moveToLeft) shouldBe expectedSeq
    moveGenerator.generate(queen, moveToLeft) shouldBe expectedSeq

    val moveToRight: Move = Move(1, 3, 7, 3)
    val expectedSeq2: Seq[Coordinates] = Seq(
      Coordinates(2, 3), Coordinates(3, 3), Coordinates(4, 3), Coordinates(5, 3), Coordinates(6, 3)
    )
    moveGenerator.generate(rook, moveToRight) shouldBe expectedSeq2
    moveGenerator.generate(queen, moveToRight) shouldBe expectedSeq2
  }

  "ClassicMoveGenerator" should "generate seq of coordinates between source and destination for diagonal move" in new TestContext {
    val moveTopRight: Move = Move(2, 7, 5 ,4)
    val expectedSeq: Seq[Coordinates] = Seq(Coordinates(3, 6), Coordinates(4, 5))
    moveGenerator.generate(bishop, moveTopRight) shouldBe expectedSeq
    moveGenerator.generate(queen, moveTopRight) shouldBe expectedSeq

    val moveTopLeft: Move = Move(5, 7, 3, 5)
    val expectedSeq2: Seq[Coordinates] = Seq(Coordinates(4, 6))
    moveGenerator.generate(bishop, moveTopLeft) shouldBe expectedSeq2
    moveGenerator.generate(queen, moveTopLeft) shouldBe expectedSeq2

    val moveBottomRight: Move = Move(3, 3, 5 ,5)
    val expectedSeq3: Seq[Coordinates] = Seq(Coordinates(4, 4))
    moveGenerator.generate(bishop, moveBottomRight) shouldBe expectedSeq3
    moveGenerator.generate(queen, moveBottomRight) shouldBe expectedSeq3

    val moveBottomLeft: Move = Move(5, 3, 2, 6)
    val expectedSeq4: Seq[Coordinates] = Seq(Coordinates(4, 4), Coordinates(3, 5))
    moveGenerator.generate(bishop, moveBottomLeft) shouldBe expectedSeq4
    moveGenerator.generate(queen, moveBottomLeft) shouldBe expectedSeq4
  }

  "ClassicMoveGenerator" should "generate empty seq if far-moving piece is moving by one field (no path between)" in new TestContext {
    val move: Move = Move(1, 5, 2, 5)
    moveGenerator.generate(rook, move) shouldBe Seq.empty
  }

  "ClassicMoveGenerator" should "generate possible moves for king from specified source" in new TestContext {
    val move: Move = Move(4, 7, 4, 6)
    val expectedMoves: Seq[Coordinates] = Seq(
      Coordinates(3, 7),
      Coordinates(3, 6),
      Coordinates(4, 6),
      Coordinates(5, 6),
      Coordinates(5, 7),
      Coordinates(5, 8),
      Coordinates(4, 8),
      Coordinates(3, 8),
    )
    moveGenerator.generate(king, move).toSet shouldBe expectedMoves.toSet
  }

  "ClassicMoveGenerator" should "generate possible moves for knight from specified source" in new TestContext {
    val move: Move = Move(2, 5, 3, 3)
    val expectedMoves: Seq[Coordinates] = Seq(
      Coordinates(1, 3),
      Coordinates(0, 4),
      Coordinates(3, 3),
      Coordinates(4, 4),
      Coordinates(4, 6),
      Coordinates(3, 7),
      Coordinates(0, 6),
      Coordinates(1, 7),
    )
    moveGenerator.generate(knight, move).toSet shouldBe expectedMoves.toSet
  }

  "ClassicMoveGenerator" should "generate possible moves for pawn from specified source" in new TestContext {
    val pawnMovingTop: Pawn = Pawn(Color.WHITE, Direction.TOP)
    val pawnMovingBottom: Pawn = Pawn(Color.BLACK, Direction.BOTTOM)

    val moveToTop: Move = Move(3, 6, 3, 5)
    val moveToBottom: Move = Move(6, 1, 6, 2)

    val expectedMovesToTop: Seq[Coordinates] = Seq(
      Coordinates(2, 5),
      Coordinates(3, 5),
      Coordinates(4, 5),
      Coordinates(3, 4)
    )

    val expectedMovesToBottom: Seq[Coordinates] = Seq(
      Coordinates(5, 2),
      Coordinates(6, 2),
      Coordinates(7, 2),
      Coordinates(6, 3),
    )

    moveGenerator.generate(pawnMovingTop, moveToTop).toSet shouldBe expectedMovesToTop.toSet
    moveGenerator.generate(pawnMovingBottom, moveToBottom).toSet shouldBe expectedMovesToBottom.toSet
  }

  "ClassicMoveGenerator" should
    "throw exception for pawn with invalid move direction (only top or bottom supported in classic)" in new TestContext {
    val move: Move = Move(1, 6, 1, 5)
    a[MoveException] shouldBe thrownBy(moveGenerator.generate(Pawn(Color.BLACK, Direction.RIGHT), move))
  }
}
