package game

sealed trait Dir {
  def opposite: Dir
}

case object North extends Dir {
  def opposite = South
}

case object NorthEast extends Dir {
  def opposite = SouthWest
}

case object East extends Dir {
  def opposite = West
}

case object SouthEast extends Dir {
  def opposite = NorthWest
}

case object South extends Dir {
  def opposite = North
}

case object SouthWest extends Dir {
  def opposite = NorthEast
}

case object West extends Dir {
  def opposite = East
}

case object NorthWest extends Dir {
  def opposite = SouthEast
}


