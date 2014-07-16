package game.tripletriad

import game.Player

case class Slot(slot: Option[(Card, Player)]) {
  lazy val isEmpty = slot.isEmpty

  def place(card: Card, by: Player): Slot = {
    copy(slot = Some(card, by))
  }

  def place(tuple: (Card, Player)): Slot = place(tuple._1, tuple._2)
  
  def flip = Slot {
    slot.map {
      case (card, player) => (card, player.other)
    }
  }

  def card: Option[Card] = slot.map(_._1)

  def player: Option[Player] = slot.map(_._2)

}

