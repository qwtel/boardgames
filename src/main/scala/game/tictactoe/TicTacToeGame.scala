package game.tictactoe

import game._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.Some

object Board {
  val Num = 3
  val Size = Pos(Num, Num)
  val ColRange = 0 until Size.x
  val RowRange = 0 until Size.y
}

case class Board(slots: Vector[Vector[Option[Player]]] = Vector.fill(3, 3)(None)) {

  import Board._

  def put(pos: Pos, player: Player): Try[Board] = {
    if (!(pos within Size)) {
      Failure(new IndexOutOfBoundsException(pos.toString))
    }
    else if (!slots(pos.x)(pos.y).isEmpty) {
      Failure(new IllegalArgumentException("Field not empty"))
    }
    else if (winner.isDefined) {
      Failure(new IllegalStateException("Game already ended"))
    }
    else {
      Success {
        Board {
          slots.updated(pos.x, slots(pos.x).updated(pos.y, Some(player)))
        }
      }
    }
  }

  def winner: Option[Player] = {

    def check(seq: Seq[Option[Player]]) = {
      val candidate = seq.head
      if (seq.count(_ == candidate) == Num) candidate
      else None
    }

    def checkCol(x: Int) = {
      val col = slots(x)
      check(col)
    }

    def checkRow(y: Int) = {
      // TODO: Allow arbitrary sizes
      val row = Seq(slots(0)(y), slots(1)(y), slots(2)(y))
      check(row)
    }

    // TODO: Allow arbitrary sizes
    def checkDiagonal = {
      val d1 = Seq(slots(0)(0), slots(1)(1), slots(2)(2))
      val d2 = Seq(slots(0)(2), slots(1)(1), slots(0)(2))
      or(check(d1), check(d2))
    }

    // TODO: Isn't there a better way?
    def or(a: Option[Player], b: Option[Player]) = {
      if (a.isDefined) a else if (b.isDefined) b else None
    }

    (ColRange.map(checkCol) ++ RowRange.map(checkRow) :+ checkDiagonal).fold(None)(or)
  }
}

case class TicTacToeMove(player: Player, pos: Pos) extends Move

case class TicTacToeGame(board: Board = Board(),
                         history: List[TicTacToeGame] = Nil,
                         activePlayer: Player = Player.random)
   extends Game[TicTacToeMove] {

  override def winner = board.winner

  override def move(move: TicTacToeMove): Try[Game[TicTacToeMove]] = {
    if (move.player != activePlayer) Failure(new IllegalArgumentException("Not your turn"))
    else {
      board.put(move.pos, move.player) map {
        board => TicTacToeGame(board, this :: history, activePlayer.other)
      }
    }
  }
}
