import game._

import game.Pos
import game.tictactoe.{Board, TicTacToeMove, TicTacToeGame}
import java.lang.IndexOutOfBoundsException
import org.scalatest.{Matchers, FlatSpec}
import scala.Some
import scala.util.Try

object TicTacToeGameTest {
  implicit def tuple2Move(t: (Player, Pos)): TicTacToeMove = {
    TicTacToeMove(t._1, t._2)
  }
  
  def wonGame = {
    Try(TicTacToeGame(activePlayer = RedPlayer))
        .flatMap(_.move((RedPlayer, Pos(0, 0))))
        .flatMap(_.move((BluePlayer, Pos(2, 2))))
        .flatMap(_.move((RedPlayer, Pos(0, 1))))
        .flatMap(_.move((BluePlayer, Pos(1, 1))))
        .flatMap(_.move((RedPlayer, Pos(0, 2))))
  }
}

class TicTacToeGameTest extends FlatSpec with Matchers {

  import TicTacToeGameTest._

  "The universe" should "exist" in {
    true should be(true)
  }

  "A tic tac toe game" should "exit" in {
    val game = TicTacToeGame(activePlayer = RedPlayer)
    game should not equal null
  }

  it should "be allow to make a move" in {
    val game = TicTacToeGame(activePlayer = RedPlayer)
    val game1 = game.move((RedPlayer, Pos(1, 1)))


    game1.isSuccess should be(true)

    game1.get match {
      case TicTacToeGame(board, _, _) =>
        board should equal {
          Board(
            Vector(
              Vector(None, None, None),
              Vector(None, Some(RedPlayer), None),
              Vector(None, None, None)
            )
          )
        }
    }

  }

  it should "forbid to place moves outside the grid" in {
    val game = TicTacToeGame(activePlayer = RedPlayer).move((RedPlayer, Pos(-1, 0)))
    game.isFailure should be(true)
    intercept[IndexOutOfBoundsException](game.get)
  }

  it should "forbid to place moves on existing moves" in {
    val game = Try(TicTacToeGame(activePlayer = RedPlayer))
        .flatMap(_.move((RedPlayer, Pos(1, 1))))
        .flatMap(_.move((BluePlayer, Pos(1, 1))))

    game.isFailure should be(true)
    intercept[IllegalArgumentException](game.get)
  }

  it should "determine a winner" in {
    val game = wonGame
    game.isSuccess should be(true)
    game.get.winner should not be None
    game.get.winner.get should equal(RedPlayer)
  }

  it should "not allow moves after a winner has been determined" in {
    val game = wonGame
        .flatMap(_.move((BluePlayer, Pos(2, 1))))

    game.isFailure should be(true)
    intercept[IllegalStateException](game.get)
  }

  it should "keep the history" in {
    val game0 = Try(TicTacToeGame(activePlayer = RedPlayer))

    val game1 = game0.flatMap(_.move((RedPlayer, Pos(0, 1))))
    val game2 = game1.flatMap(_.move((BluePlayer, Pos(1, 1))))

    game2.get.history.head should equal(game1.get)
    game1.get.history.head should equal(game0.get)
  }

  it should "not allow a player to go twice" in {
    val game0 = Try(TicTacToeGame(activePlayer = RedPlayer))
    val game = game0
        .flatMap(_.move((RedPlayer, Pos(0, 0))))
        .flatMap(_.move((RedPlayer, Pos(1, 1))))

    game.isFailure should be(true)
    intercept[IllegalArgumentException](game.get)
  }

}
