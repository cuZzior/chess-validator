package cuzz.chessvalidator
import com.typesafe.scalalogging.Logger
import cuzz.chessvalidator.Color.Color
import cuzz.chessvalidator.boards.Board
import cuzz.chessvalidator.fields.Field
import cuzz.chessvalidator.moveGenerator.{ClassicMoveGenerator, MoveGenerator}
import cuzz.chessvalidator.pieces._

import scala.collection.immutable.Seq
import scala.util.{Failure, Success, Try}

class MoveValidator(moveGenerator: MoveGenerator) {
  val logger: Logger = Logger("MoveValidator")
  def validate(move: Move, board: Board, currentPlayerColor: Color): Unit = {
    val sourceSquare = board.getFieldBy(move.source)
      .getOrElse(throw new MoveException("Source square out of bounds."))
    val destinationSquare = board.getFieldBy(move.destination)
      .getOrElse(throw new MoveException("Destination square out of bounds."))

    if (!sourceSquare.hasPiece)
      throw new MoveException("Source square empty.")
    if (move.source == move.destination)
      throw new MoveException("Piece has to be moved from source location.")
    if (sourceSquare.piece.get.color != currentPlayerColor)
      throw new MoveException("Piece is different color than current player.")
    if (destinationSquare.hasPiece && destinationSquare.piece.get.color == currentPlayerColor)
      throw new MoveException("Destination square is taken by your other piece.")

    val pieceToMove: Piece = sourceSquare.piece.get
    val possibleMoves: Seq[Coordinates] = moveGenerator.generate(pieceToMove, move)
    sourceSquare.piece.get match {
      case _: Rook | _: Bishop | _: Queen => checkPathForBlockingPieces(possibleMoves, board)
      case pawn: Pawn => validatePawnMove(possibleMoves, pawn, move, board)
      case _: Knight => validateKnightMove(possibleMoves, move)
      case _: King => validateKingMove(possibleMoves, move)
      case _ => throw new MoveException("Piece not supported.")
    }

    isKingInCheckAfterMove(move, board, currentPlayerColor)
  }

  def isCurrentPlayerKingInCheck(board: Board, currentPlayerColor: Color): Boolean = {
    val kingLocation: Field = board.fields.filter { square =>
      square.hasPiece && square.piece.get.color == currentPlayerColor && square.piece.get.isInstanceOf[King]
    }.head

    val canOpponentMoveCheckKingList: Seq[Boolean] = board.fields.filter{ field =>
      field.hasPiece && field.piece.get.color != currentPlayerColor
    }.map { square =>
      Try(
        validate(Move(square.coordinates, kingLocation.coordinates), board, Color.opponentColor(currentPlayerColor))
      ) match {
        case Failure(_: MoveException) => false
        case Success(_) => true
      }
    }
    canOpponentMoveCheckKingList.contains(true)
  }

  private def isKingInCheckAfterMove(move: Move, board: Board, currentPlayerColor: Color): Unit = {
    val pieceToMove = board.getFieldBy(move.source).get.piece.get
    val boardStateAfterMove = board
      .modify(board.getFieldBy(move.source).get.removePiece())
      .modify(board.getFieldBy(move.destination).get.addPiece(pieceToMove))

    if (isCurrentPlayerKingInCheck(boardStateAfterMove, currentPlayerColor))
      throw new MoveException(s"$currentPlayerColor king still in check.")
  }

  private def checkPathForBlockingPieces(coordinatesBetweenSourceAndDest: Seq[Coordinates], boardState: Board): Unit = {
    coordinatesBetweenSourceAndDest.foreach { coordinate =>
      if (boardState.getFieldBy(coordinate).get.hasPiece)
        throw new MoveException("Invalid move. Another piece is blocking path.")
    }
  }

  private def validatePawnMove(possiblePawnMoves: Seq[Coordinates], pawn: Pawn, move: Move, board: Board): Unit = {
    if (!possiblePawnMoves.contains(move.destination))
      throw new MoveException("Invalid pawn move.")
    if (
      move.source.column != move.destination.column &&
      !board.getFieldBy(move.destination).get.hasPiece
    )
      throw new MoveException("Cannot move diagonally without attack.")

    if (pawn.hasMoved && (move.source.row - move.destination.row).abs != 1)
      throw new MoveException("Cannot move by two squares on subsequent move.")
  }

  private def validateKnightMove(possibleKnightMoves: Seq[Coordinates], move: Move): Unit = {
    if (!possibleKnightMoves.contains(move.destination))
      throw new MoveException("Invalid knight move.")
  }

  private def validateKingMove(possibleKingMoves: Seq[Coordinates], move: Move): Unit = {
    if (!possibleKingMoves.contains(move.destination))
      throw new MoveException("Invalid king move.")
  }
}

object MoveValidator {
  def classicWHGSimplified = new MoveValidator(new ClassicMoveGenerator)
}

class MoveException(message: String) extends Exception(message)