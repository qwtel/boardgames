package game

object Dir extends Enumeration {
  type Dir = Value
  val North, East, South, West = Value

  case class DirValue(opposite: Dir)

  implicit def value2DirValue(dir: Dir) = dir match {
    case North => DirValue(South)
    case East => DirValue(West)
    case South => DirValue(North)
    case West => DirValue(East)
  }
}

object Dir8 extends Enumeration {
  type Dir8 = Value
  val North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest = Value

  case class DirValue(opposite: Dir8)

  implicit def value2DirValue(dir: Dir8) = dir match {
    case North => DirValue(South)
    case NorthEast => DirValue(SouthWest)
    case East => DirValue(West)
    case SouthEast => DirValue(NorthWest)
    case South => DirValue(North)
    case SouthWest => DirValue(NorthEast)
    case West => DirValue(East)
    case NorthWest => DirValue(SouthEast)
  }
}
