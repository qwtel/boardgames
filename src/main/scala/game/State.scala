package game

import scala.util.Try

/**
 * Represents the state of a game.
 * 
 * @tparam M The type of move that is allowed in this game
 */
trait State[M <: Move] {

  /**
   * @return The player who's turn it is
   */
  def activePlayer: Player

  /**
   * @param move A move by a player
   * @return A success of a new game if the move is valid, or failure otherwise
   */
  def move(move: M): Try[State[M]]

  /**
   * @return The winner of the game, none otherwise
   */
  def winner: Option[Player]

  /**
   * @return A list of previous states of this game.
   *         Its head is the previous state of the game (before this one)
   */
  def history: List[State[M]]
}
