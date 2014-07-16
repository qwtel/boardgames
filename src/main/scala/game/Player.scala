package game

import scala.util.Random

object Player {
  def random: Player = if ((new Random).nextBoolean()) RedPlayer else BluePlayer
}

trait Player {
  def other: Player
}

case object RedPlayer extends Player {
  def other = BluePlayer
}

case object BluePlayer extends Player {
  def other = RedPlayer
}

