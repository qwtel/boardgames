package game

sealed trait Dir {
  def opposite: Dir
}

case object North extends Dir {
  def opposite = South
}

case object South extends Dir {
  def opposite = North
}

case object East extends Dir {
  def opposite = West
}

case object West extends Dir {
  def opposite = East
}

