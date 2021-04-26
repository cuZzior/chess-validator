package cuzz.chessvalidator

import com.typesafe.scalalogging.Logger
import cuzz.chessvalidator.Color.Color
import cuzz.chessvalidator.boards.{Board, ClassicBoard}
import cuzz.chessvalidator.moveGenerator.ClassicMoveGenerator
import cuzz.chessvalidator.pieces.{Pawn, Piece}

import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.util.{Failure, Success, Try}
case class Game(moveValidator: MoveValidator, board: Board, currentPlayerColor: Color = Color.WHITE) {
  val logger: Logger = Logger("Game")

  def playProvidedMoves(moves: Seq[Move]): Game = {
    board.renderBoard()
    executeMoves(moves)
  }

  @tailrec
  private def executeMoves(moves: Seq[Move], game: Game = this): Game = {
    if (moves.isEmpty) {
      logger.info("Moves executed.")
      game
    }
    else executeMoves(moves.tail, game.makeMove(moves.head))
  }

  private def makeMove(move: Move): Game = {
    if (moveValidator.isCurrentPlayerKingInCheck(board, currentPlayerColor)) logger.info(s"Player $currentPlayerColor starts in check.")

    Try(moveValidator.validate(move, board, currentPlayerColor)) match {
      case Failure(ruleException: MoveException) =>
        logger.info(s"INVALID MOVE $move: ${ruleException.getMessage}")
        this
      case Success(_) =>
        val sourceSquare = board.getFieldBy(move.source).get
        val destinationSquare = board.getFieldBy(move.destination).get
        val piece: Piece = sourceSquare.piece.get match {
          case pawn: Pawn => pawn.copy(hasMoved = true)
          case piece: Piece => piece
        }

        val newBoardState = board
          .modify(sourceSquare.removePiece())
          .modify(destinationSquare.addPiece(piece))

        logger.info(s"$currentPlayerColor moved $move")
        newBoardState.renderBoard()
        this
          .applyNewBoardState(newBoardState)
          .passToSecondPlayer
    }
  }

  private def applyNewBoardState(newBoardState: Board): Game = {
    this.copy(board = newBoardState)
  }

  private def passToSecondPlayer: Game = {
    currentPlayerColor match {
      case Color.WHITE => this.copy(currentPlayerColor = Color.BLACK)
      case Color.BLACK => this.copy(currentPlayerColor = Color.WHITE)
    }
  }
}

object Game {
  def simplifiedWHGClassicChess(): Game = new Game(new MoveValidator(new ClassicMoveGenerator), ClassicBoard())
}
