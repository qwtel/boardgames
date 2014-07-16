package game

import game.Dir._
import game.Dir8.Dir8

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
    case East => east
    case South => south
    case West => west
  }

  def ofDir8(dir: Dir8) = dir match {
    case Dir8.North => north
    case Dir8.NorthEast => northEast
    case Dir8.East => east
    case Dir8.SouthEast => southEast
    case Dir8.South => south
    case Dir8.SouthWest => southWest
    case Dir8.West => west
    case Dir8.NorthWest => northWest
  }

  def within(width: Int, height: Int): Boolean = 0 <= x && x < width && 0 <= y && y < height

  def within(size: Pos): Boolean = within(size.x, size.y)
}

