package twentyone.thirteen

import lib.{Coord, Support}

object TwentyoneThirteen extends App with Support {
  def doFold(dots: Set[Coord], fold: String): Set[Coord] = {
    fold match {
      case s"fold along x=${dx}" =>
        val mx = dx.toInt
        dots.map {
          case Coord(x, y) if mx < x => Coord(2 * mx - x, y)
          case coord                 => coord
        }
      case s"fold along y=${dy}" =>
        val my = dy.toInt
        dots.map {
          case Coord(x, y) if my < y => Coord(x, 2 * my - y)
          case coord                 => coord
        }
    }
  }
  val i = loadStringSeq

  val sep = i.indexOf("")

  val dots = i
    .slice(0, sep)
    .map { case s"$x,$y" => Coord(x.toInt, y.toInt) }
    .toSet
  val folds = i.slice(sep + 1, i.size)

  val part1 = doFold(dots, folds.head).size

  val folded = folds.foldLeft(dots)(doFold)

  val maxX = folded.map(_.x).max
  val maxY = folded.map(_.y).max

  val part2 = (0 to maxY)
    .map(y => (0 to maxX).map(x => if (folded.contains(Coord(x, y))) '#' else ' ').mkString)
    .mkString("\n")

  println(part1)
  println(part2)
}
