package game

import scala.util.Try


trait Game[M <: Move] {
  def activePlayer: Player
  
  def move(move: M): Try[Game[M]]

  def winner: Option[Player]

  def history: List[Game[M]]
}
