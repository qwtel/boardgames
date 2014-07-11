package game

import scala.util.Random

object Player {
  def random = if ((new Random).nextBoolean()) RedPlayer else BluePlayer
}

sealed trait Player {
  def other: Player
}

case object RedPlayer extends Player {
  def other = BluePlayer
}

case object BluePlayer extends Player {
  def other = RedPlayer
}

