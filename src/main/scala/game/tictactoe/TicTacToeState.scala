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

/**
 * A 2D vector that contains an option of a player object.
 * 
 * If it is Some(Player) it means that the player has set his mark there.
 * None means the slot is empty.
 * 
 * @param slots A 2D vector representing the state of the board 
 */
case class Board(slots: Vector[Vector[Option[Player]]] = Vector.fill(3, 3)(None)) {

  import Board._

  /**
   * Lets a player put his mark at a position
   * @param pos the position where the mark should be set
   * @param player the player that wants to set the mark
   * @return a Success of a new Board if the move is allowed, Failure otherwise
   */
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

  /**
   * Determines the winner of the game.
   * @return Some player if the game is over, None otherwise
   */
  def winner: Option[Player] = {

    /**
     * Checks if all fields are of the same player in a sequence.
     * @param seq A sequence of fields that are either empty or contains the mark of some player
     * @return Some(Player) if the sequence 
     */
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

    // creates a sequence of options, then checks if at least one of them is defined
    // the way the game works there is at most one options that is defined
    (ColRange.map(checkCol) ++ RowRange.map(checkRow) :+ checkDiagonal).fold(None)(or)
  }
}

/**
 * A move in the tic tac toe game
 * @param player The player who wants to make the move
 * @param pos The position where the player wants to make his mark
 */
case class TicTacToeMove(player: Player, pos: Pos) extends Move

object TicTacToeState {
  implicit def tuple2Move(t: (Player, Pos)): TicTacToeMove = {
    TicTacToeMove(t._1, t._2)
  }
}

case class TicTacToeState(board: Board = Board(),
                         history: List[TicTacToeState] = Nil,
                         activePlayer: Player = Player.random)
   extends State[TicTacToeMove] {
  
  override def winner = board.winner

  override def move(move: TicTacToeMove): Try[State[TicTacToeMove]] = {
    if (move.player != activePlayer) Failure(new IllegalArgumentException("Not your turn"))
    else {
      board.put(move.pos, move.player) map {
        board => TicTacToeState(board, this :: history, activePlayer.other)
      }
    }
  }
}

class TicTacToeGame {
  var state: State[TicTacToeMove] = TicTacToeState()
  def move(move: TicTacToeMove) = {
    val nextState = state.move(move)
    if (nextState.isSuccess) state = nextState.get
    else nextState.failed.foreach(ex => println(ex.getMessage))
  }
}
