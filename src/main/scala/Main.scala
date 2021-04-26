import cuzz.chessvalidator.{Game, Move, UserInputFile}

import scala.annotation.tailrec
import scala.collection.immutable.Seq

object Main extends App {
  val game = Game.simplifiedWHGClassicChess()
  val userInputFile = new UserInputFile("src/main/data/sample-moves.txt")

  @tailrec
  def readMovesFromFile(accumulator: Seq[Move] = List.empty): Seq[Move] = {
    val move: Option[Array[Int]] = Option(userInputFile.nextMove())
    move match {
      case None => accumulator
      case Some(move) => readMovesFromFile(accumulator.:+(Move(move)))
    }
  }
  game.playProvidedMoves(readMovesFromFile())
}