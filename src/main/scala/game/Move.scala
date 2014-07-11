package game

/**
 * Base trait for game moves.
 * Must at least contain the player that undertakes that move.
 * Subclasses should contain other data about the move, like position, played card, etc..
 */
trait Move {
  def player: Player
}
