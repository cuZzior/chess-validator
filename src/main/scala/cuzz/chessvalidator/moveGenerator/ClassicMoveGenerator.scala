package cuzz.chessvalidator.moveGenerator

import cuzz.chessvalidator.pieces._
import cuzz.chessvalidator.{Coordinates, Direction, Move, MoveException}

import scala.annotation.tailrec
import scala.collection.immutable.Seq

class ClassicMoveGenerator extends MoveGenerator {

  def generate(piece: Piece, move: Move): Seq[Coordinates] = {
    piece match {
      case rook: Rook => generatePathBetweenSourceAndDest(rook, move)
      case bishop: Bishop => generatePathBetweenSourceAndDest(bishop, move)
      case queen: Queen => generatePathBetweenSourceAndDest(queen, move)
      case pawn: Pawn => generatePossiblePawnMoves(pawn, move)
      case _: Knight => generatePossibleKnightMoves(move)
      case _: King => generatePossibleKingMoves(move)
      case _  => throw new GeneratorException("Piece not supported.")
    }
  }

  private def generatePossiblePawnMoves(pawn: Pawn, move: Move): Seq[Coordinates]= {
    pawn.movingDirection match {
      case Direction.TOP =>
        Seq(
          Coordinates(move.source.column - 1, move.source.row - 1),
          Coordinates(move.source.column, move.source.row - 1),
          Coordinates(move.source.column + 1, move.source.row - 1),
          Coordinates(move.source.column, move.source.row - 2)
        )
      case Direction.BOTTOM =>
        Seq(
          Coordinates(move.source.column - 1, move.source.row + 1),
          Coordinates(move.source.column, move.source.row + 1),
          Coordinates(move.source.column + 1, move.source.row + 1),
          Coordinates(move.source.column, move.source.row + 2)
        )
      case _ => throw new MoveException(s"Invalid moving direction for ${pawn.getClass.getSimpleName}.")
    }
  }
  private def generatePossibleKnightMoves(move: Move): Seq[Coordinates] = {
    val sourceCords: Coordinates = move.source
    Seq(
      Coordinates(sourceCords.column + 1, sourceCords.row - 2),
      Coordinates(sourceCords.column + 1, sourceCords.row + 2),
      Coordinates(sourceCords.column - 1, sourceCords.row + 2),
      Coordinates(sourceCords.column - 1, sourceCords.row - 2),
      Coordinates(sourceCords.column + 2, sourceCords.row - 1),
      Coordinates(sourceCords.column + 2, sourceCords.row + 1),
      Coordinates(sourceCords.column - 2, sourceCords.row - 1),
      Coordinates(sourceCords.column - 2, sourceCords.row + 1)
    )
  }

  private def generatePossibleKingMoves(move: Move): Seq[Coordinates] = {
    val sourceCords: Coordinates = move.source
    Seq(
      Coordinates(sourceCords.column, sourceCords.row - 1),
      Coordinates(sourceCords.column, sourceCords.row + 1),
      Coordinates(sourceCords.column - 1, sourceCords.row),
      Coordinates(sourceCords.column + 1, sourceCords.row),
      Coordinates(sourceCords.column + 1, sourceCords.row - 1),
      Coordinates(sourceCords.column + 1, sourceCords.row + 1),
      Coordinates(sourceCords.column - 1, sourceCords.row + 1),
      Coordinates(sourceCords.column - 1, sourceCords.row - 1)
    )
  }

  private def generatePathBetweenSourceAndDest(piece: Piece, move: Move): Seq[Coordinates] = {
    val direction = Direction.defineMoveDirection(move)
    (piece, direction) match {
      case (_: Rook | _: Queen, Direction.TOP) =>
        generateVertical(move, Top.nextRowFunc, Top.conditionFunc)
      case (_: Rook | _: Queen, Direction.RIGHT) =>
        generateHorizontal(move, Right.nextColumnFunc, Right.conditionFunc)
      case (_: Rook | _: Queen, Direction.BOTTOM) =>
        generateVertical(move, Bottom.nextRowFunc, Bottom.conditionFunc)
      case (_: Rook | _: Queen, Direction.LEFT) =>
        generateHorizontal(move, Left.nextColumnFunc, Left.conditionFunc)
      case (_: Bishop | _: Queen, Direction.TOP_RIGHT) =>
        generateDiagonal(move, TopRight.nextColumnFunc, TopRight.nextRowFunc, TopRight.conditionFunc)
      case (_: Bishop | _: Queen, Direction.BOTTOM_RIGHT) =>
        generateDiagonal(move, BottomRight.nextColumnFunc, BottomRight.nextRowFunc, BottomRight.conditionFunc)
      case (_: Bishop | _: Queen, Direction.BOTTOM_LEFT) =>
        generateDiagonal(move, BottomLeft.nextColumnFunc, BottomLeft.nextRowFunc, BottomLeft.conditionFunc)
      case (_: Bishop | _: Queen, Direction.TOP_LEFT) =>
        generateDiagonal(move, TopLeft.nextColumnFunc, TopLeft.nextRowFunc, TopLeft.conditionFunc)
      case _ => throw new MoveException(s"Invalid move for ${piece.getClass.getSimpleName}.")
      }
    }

  private def generateVertical(move: Move, nextRowFunc: Int => Int, conditionFunc: (Int, Move) => Boolean): Seq[Coordinates] = {
    @tailrec
    def aux(rowNumber: Int, squareAccumulator: Seq[Coordinates] = Seq.empty): Seq[Coordinates] = {
      val nextRow = nextRowFunc(rowNumber)
      if (conditionFunc(nextRow, move)) squareAccumulator
      else aux(nextRow, squareAccumulator :+ Coordinates(move.source.column, nextRow))
    }
    aux(move.source.row)
  }

  private def generateHorizontal(move: Move, nextColumnFunc: Int => Int, conditionFunc: (Int, Move) => Boolean): Seq[Coordinates] = {
    @tailrec
    def aux(columnNumber: Int, squareAccumulator: Seq[Coordinates] = Seq.empty): Seq[Coordinates] = {
      val nextColumn = nextColumnFunc(columnNumber)
      if (conditionFunc(nextColumn, move)) squareAccumulator
      else aux(nextColumn, squareAccumulator :+ Coordinates(nextColumn, move.source.row))
    }
    aux(move.source.column)
  }

  private def generateDiagonal(move: Move, nextColumnFunc: Int => Int, nextRowFunc: Int => Int, conditionFunc: (Int, Int, Move) => Boolean): Seq[Coordinates] = {
    @tailrec
    def aux(columnNumber: Int, rowNumber: Int, squareAccumulator: Seq[Coordinates] = Seq.empty): Seq[Coordinates] = {
      val nextColumn = nextColumnFunc(columnNumber)
      val nextRow = nextRowFunc(rowNumber)
      if (conditionFunc(nextColumn, nextRow, move)) squareAccumulator
      else aux(nextColumn, nextRow, squareAccumulator :+ Coordinates(nextColumn, nextRow))
    }
    aux(move.source.column, move.source.row)
  }

  private object Top {
    val nextRowFunc: Int => Int = (row: Int) => row - 1
    val conditionFunc: (Int, Move) => Boolean = {
      (row: Int, move: Move) => {
        row <= move.destination.row
      }
    }
  }

  private object Bottom {
    val nextRowFunc: Int => Int = (row: Int) => row + 1
    val conditionFunc: (Int, Move) => Boolean = {
      (row: Int, move: Move) => {
        row >= move.destination.row
      }
    }
  }

  private object Right {
    val nextColumnFunc: Int => Int = (column: Int) => column + 1
    val conditionFunc: (Int, Move) => Boolean = {
      (column: Int, move: Move) => {
        column >= move.destination.column
      }
    }
  }

  private object Left {
    val nextColumnFunc: Int => Int = (column: Int) => column - 1
    val conditionFunc: (Int, Move) => Boolean = {
      (column: Int, move: Move) => {
        column <= move.destination.column
      }
    }
  }

  private object TopRight {
    val nextColumnFunc: Int => Int = (column: Int) => column + 1
    val nextRowFunc: Int => Int = (row: Int) => row - 1
    val conditionFunc: (Int, Int, Move) => Boolean = {
      (column: Int, row: Int, move: Move) => {
        column >= move.destination.column || row <= move.destination.row
      }
    }
  }

  private object TopLeft {
    val nextColumnFunc: Int => Int = (column: Int) => column - 1
    val nextRowFunc: Int => Int = (row: Int) => row - 1
    val conditionFunc: (Int, Int, Move) => Boolean = {
      (column: Int, row: Int, move: Move) => {
        column <= move.destination.column || row <= move.destination.row
      }
    }
  }

  private object BottomLeft {
    val nextColumnFunc: Int => Int = (column: Int) => column - 1
    val nextRowFunc: Int => Int = (row: Int) => row + 1
    val conditionFunc: (Int, Int, Move) => Boolean = {
      (column: Int, row: Int, move: Move) => {
        column <= move.destination.column || row >= move.destination.row
      }
    }
  }

  private object BottomRight {
    val nextColumnFunc: Int => Int = (column: Int) => column + 1
    val nextRowFunc: Int => Int = (row: Int) => row + 1
    val conditionFunc: (Int, Int, Move) => Boolean = {
      (column: Int, row: Int, move: Move) => {
        column >= move.destination.column || row >= move.destination.row
      }
    }
  }
}

class GeneratorException(message: String) extends Exception(message)