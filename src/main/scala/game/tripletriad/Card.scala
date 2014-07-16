package game.tripletriad

import game.Dir._

case class Card(top: Int, right: Int, bottom: Int, left: Int) {
  def rankAt(dir: Dir) = dir match {
    case North => top
    case East => left
    case South => bottom
    case West => right
  }
  
  def level = (top + right + bottom + left) / 4
}

