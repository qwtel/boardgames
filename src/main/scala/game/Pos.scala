package game

case class Pos(x: Int, y: Int) {
  def north = Pos(x - 1, y)

  def south = Pos(x + 1, y)

  def east = Pos(x, y + 1)

  def west = Pos(x, y - 1)

  def of(dir: Dir) = dir match {
    case North => north
    case South => south
    case East => east
    case West => west
  }

  def within(width: Int, height: Int): Boolean = 0 <= x && x < width && 0 <= y && y < height

  def within(size: Pos): Boolean = within(size.x, size.y)
}

