import lib.{Coord, Support}

import scala.annotation.tailrec

object d9 extends App with Support {
  val i = load2dIntSeq(delimiterB = "")

  val heightmap = (for {
    (row, y) <- i.zipWithIndex
    (height, x) <- row.zipWithIndex
  } yield Coord(x, y) -> height).toMap

  val lowpoints = heightmap.filter { case (coord, height) =>
    coord.neighbours.filter(heightmap.contains).forall(heightmap(_) > height)
  }

  val part1 = lowpoints.map(_._2 + 1).sum

  println(part1)

  @tailrec def mapBasin(q: List[Coord], found: Set[Coord]): Int = {
    q match {
      case Nil                                  => found.size
      case head :: rest if heightmap(head) == 9 => mapBasin(rest, found)
      case head :: rest =>
        val next = head.cardinalNeighbours.filter(heightmap.contains).filterNot(found.contains).toList
        mapBasin(next ++ rest, found + head)
    }
  }

  val basins = lowpoints.keys.toSeq.map(lowpoint => {
    mapBasin(List(lowpoint), Set.empty)
  })

  val part2 = basins.sorted.reverse.take(3).product

  println(part2)
}
