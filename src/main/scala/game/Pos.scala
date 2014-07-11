package game

case class Pos(x: Int, y: Int) {
  lazy val north = Pos(x - 1, y)

  lazy val northEast = Pos(x - 1, y + 1)

  lazy val east = Pos(x, y + 1)

  lazy val southEast = Pos(x + 1, y + 1)

  lazy val south = Pos(x + 1, y)

  lazy val southWest = Pos(x + 1, y - 1)

  lazy val west = Pos(x, y - 1)

  lazy val northWest = Pos(x - 1, y - 1)

  def of(dir: Dir) = dir match {
    case North => north
    case NorthEast => northEast
    case East => east
    case SouthEast => southEast
    case South => south
    case SouthWest => southWest
    case West => west
    case NorthWest => northWest
  }

  def within(width: Int, height: Int): Boolean = 0 <= x && x < width && 0 <= y && y < height

  def within(size: Pos): Boolean = within(size.x, size.y)
}

