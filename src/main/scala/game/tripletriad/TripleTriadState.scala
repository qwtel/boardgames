package game.tripletriad

import game._
import scala.util.{Success, Try, Failure}
import game.Pos
import game.Dir._

case class TripleTriadMove(player: Player, card: Card, pos: Pos) extends Move

object TripleTriadState {
  val N = 3
  val Size = Pos(N, N)

  implicit def tuple2Move(t: (Player, Card, Pos)): TripleTriadMove = {
    TripleTriadMove(t._1, t._2, t._3)
  }

}

case class TripleTriadState(board: Map[Pos, (Card, Player)] = Map(),
                            history: List[TripleTriadState] = List(),
                            currPlayer: Player = Player.random)
    extends State[TripleTriadMove] {

  import TripleTriadState._

  override def winner: Option[Player] = ???

  override def move(move: TripleTriadMove): Try[State[TripleTriadMove]] = {
    if (currPlayer != move.player) Failure(new IllegalArgumentException("Not your turn"))
    else {
      tripleTriadMove(move.player, move.card, move.pos) map {
        board => TripleTriadState(board, this :: history, currPlayer.other)
      }
    }
  }

  private def tripleTriadMove(player: Player, card: Card, pos: Pos): Try[Map[Pos, (Card, Player)]] = {
    if (!(pos within Size)) {
      Failure(new IndexOutOfBoundsException(pos.toString))
    } else if (board.get(pos).isDefined) {
      Failure(new IllegalArgumentException("Field not empty"))
    } else {
      val nextBoard = board.updated(pos, (card, player))

      def flip(board: Map[Pos, (Card, Player)], dir: Dir): Map[Pos, (Card, Player)] = {
        val otherPos = pos.of(dir)
        val otherSlot: Option[(Card, Player)] = board.get(otherPos)
        otherSlot map {
          case (otherCard, otherPlayer) if otherPlayer != player && card.rankAt(dir) >= otherCard.rankAt(dir.opposite) =>
            board.updated(otherPos, (otherCard, player))
        } getOrElse {
          board
        }
      }

      Success {
        Dir.values.foldLeft(nextBoard)(flip)
      }
    }
  }
}
