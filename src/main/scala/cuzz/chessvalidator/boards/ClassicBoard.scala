package cuzz.chessvalidator.boards

import com.typesafe.scalalogging.Logger
import cuzz.chessvalidator.Color.Color
import cuzz.chessvalidator.pieces._
import cuzz.chessvalidator._
import cuzz.chessvalidator.fields.{Field, Square}

import scala.collection.immutable.Seq

case class ClassicBoard(fields: Seq[Field]) extends Board {
  val logger: Logger = Logger("Board")

   def modify(field: Field): ClassicBoard = {
    val indexOfFieldWithinCollection = fields.indexWhere(_.coordinates == field.coordinates)
    this.copy(fields.updated(indexOfFieldWithinCollection, field))
  }

  def getFieldBy(coordinates: Coordinates): Option[Field] = {
    fields.find(_.coordinates == coordinates)
  }

  def renderBoard(): Unit = {
    fields.grouped(8).toSeq
      .foreach{seq =>
        println(seq.mkString("| "," ", " |"))
      }
    println("\n\n")
  }
}

object ClassicBoard {
  def apply(): ClassicBoard = new ClassicBoard(
    generateTopPlayerSquares(Color.BLACK) ++ generateEmptySquares ++ generateBottomPlayerSquares(Color.WHITE)
  )

  private def generateTopPlayerSquares(color: Color): Seq[Square] = {
    Seq(
      Square(0, 0, Rook(color)),
      Square(1, 0, Knight(color)),
      Square(2, 0, Bishop(color)),
      Square(3, 0, Queen(color)),
      Square(4, 0, King(color)),
      Square(5, 0, Bishop(color)),
      Square(6, 0, Knight(color)),
      Square(7, 0, Rook(color)),
    ) ++ generateRowOfSquares(1, Some(Pawn(color, Direction.BOTTOM)))
  }

  private def generateBottomPlayerSquares(color: Color): Seq[Square] = {
    generateRowOfSquares(6, Some(Pawn(color, Direction.TOP))) ++
      Seq(
        Square(0, 7, Rook(color)),
        Square(1, 7, Knight(color)),
        Square(2, 7, Bishop(color)),
        Square(3, 7, Queen(color)),
        Square(4, 7, King(color)),
        Square(5, 7, Bishop(color)),
        Square(6, 7, Knight(color)),
        Square(7, 7, Rook(color)),
      )
  }

  private def generateEmptySquares: Seq[Square] = {
    val rows = for {
      rowNumber <- 2 to 5
    } yield generateRowOfSquares(rowNumber, None)
    rows.flatten
  }

  private def generateRowOfSquares(rowNumber: Int, piece: Option[Piece]): Seq[Square] = {
    for {
      columnNumber <- 0 to 7
    } yield Square(columnNumber, rowNumber, piece)
  }
}