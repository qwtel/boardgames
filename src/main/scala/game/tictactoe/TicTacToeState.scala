package game.tictactoe

import game._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.Some

/**
 * A move in the tic tac toe game.
 *
 * @param player The player who wants to make the move
 * @param pos The position where the player wants to make his mark
 */
case class TicTacToeMove(player: Player, pos: Pos) extends Move

object TicTacToeState {

  val N = 3
  val Size = Pos(N, N)

  implicit def tuple2Move(t: (Player, Pos)): TicTacToeMove = {
    TicTacToeMove(t._1, t._2)
  }
  
  /**
   * Number required to win
   */
  val WinNum = 3
}

/**
 * Represents the NxN board of a tic tac toe game.
 *
 * If a slot is Some(Player) it means that this player has set its mark there.
 * None means the slot is empty.
 *
 * @param board A 2D seq representing the board 
 * @param history The history of the game
 * @param currPlayer The player who's turn it is
 */
case class TicTacToeState(board: Seq[Seq[Option[Player]]] = Seq.fill(3, 3)(None),
                          history: List[TicTacToeState] = Nil,
                          currPlayer: Player = Player.random)
    extends State[TicTacToeMove] {

  import TicTacToeState._

  override def move(move: TicTacToeMove): Try[State[TicTacToeMove]] = {
    if (currPlayer != move.player) Failure(new IllegalArgumentException("Not your turn"))
    else {
      ticTacToeMove(move.pos, move.player) map {
        slots => TicTacToeState(slots, this :: history, currPlayer.other)
      }
    }
  }

  private def ticTacToeMove(pos: Pos, player: Player): Try[Seq[Seq[Option[Player]]]] = {
    if (!(pos within Size)) {
      Failure(new IndexOutOfBoundsException(pos.toString))
    }
    else if (board(pos.x)(pos.y).isDefined) {
      Failure(new IllegalArgumentException("Field not empty"))
    }
    else if (winner.isDefined) {
      Failure(new IllegalStateException("Game already ended"))
    }
    else {
      Success(
        board.updated(pos.x, board(pos.x).updated(pos.y, Some(player)))
      )
    }
  }

  /**
   * Determines the winner of the game.
   * @return Some player if the game is over, None otherwise
   */
  override def winner: Option[Player] = {

    /**
     * Extract a column
     * @param x the number of the column, 0 until N
     */
    def |(x: Int): Seq[Option[Player]] = {
      board(x)
    }

    /**
     * Extract a row
     * @param y the number of the row, 0 until N
     */
    def --(y: Int): Seq[Option[Player]] = {
      for (x <- 0 until Size.x) yield board(x)(y)
    }

    /**
     * Extract a / diagonal in this direction
     * @param p The number of the diagonal, 0 until 2 * N - 1
     */
    def /(p: Int): Seq[Option[Player]] = {
      for (q <- (0 max p - N + 1) to (p min N - 1)) yield board(q)(p - q)
    }

    /**
     * Extract a \ diagonal in this direction: \
     * @param p The number of the diagonal, 0 until 2 * N - 1
     */
    def \(p: Int): Seq[Option[Player]] = {
      for (q <- (0 max p - N + 1) to (p min N - 1)) yield board(N - 1 - q)(p - q)
    }

    /**
     * Checks if all fields are of the same player in a sequence.
     *
     * @param func A function that generates a row, col or diagonal of the board
     * @param i The number of the row, col or diagonal
     * @return some player if all fields in the row, col or diagonal contain this player
     */
    def check(func: (Int) => Seq[Option[Player]])(i: Int) = {
      val seq = func(i)
      val candidate = seq.head
      if (seq.count(_ == candidate) == WinNum) candidate
      else None
    }

    // creates a sequence of options, then checks if at least one of them is defined
    // the way the game works there is at most one option that is not empty
    val checkedCols = (0 until Size.x) map check(|)
    val checkedRows = (0 until Size.y) map check(--)
    val checkedDiagonalsAsc = (0 until 2 * N - 1) map check(/)
    val checkedDiagonalsDesc = (0 until 2 * N - 1) map check(\)

    (checkedCols ++ checkedRows ++ checkedDiagonalsAsc ++ checkedDiagonalsDesc).fold(None) {
      (acc, elem) => if (acc.isDefined) acc else elem
    }
  }
}

