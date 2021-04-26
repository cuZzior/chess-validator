package cuzz.chessvalidator

import org.scalatest.{FlatSpec, Matchers}

class GameTest extends FlatSpec with Matchers {

  trait TestContext {
    val game: Game = Game.simplifiedWHGClassicChess()
  }

  "Game" should "return current board for invalid move" in new TestContext {
    val gameAfterMove: Game = game.playProvidedMoves(Seq(Move(2, 7, 2, 6)))
    game.board shouldBe gameAfterMove.board
  }

  "Game" should "return modified board for valid move" in new TestContext {
    val gameAfterMove: Game = game.playProvidedMoves(Seq(Move(1, 6, 1, 4)))
    game.board should not be gameAfterMove.board
  }

  "Game" should "pass turn to other player" in new TestContext {
    val gameAfterMove: Game = game.playProvidedMoves(Seq(Move(1, 6, 1, 4)))
    gameAfterMove.currentPlayerColor should not be game.currentPlayerColor
  }
}
