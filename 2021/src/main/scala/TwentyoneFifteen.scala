package twentyone.fifteen

import lib.{Coord, Support}

import scala.annotation.tailrec
import scala.collection.mutable

final case class Path(position: Coord, risk: Int)

object Path {
  implicit object PathOrdering extends Ordering[Path] {
    override def compare(x: Path, y: Path): Int = -(x.risk compare y.risk)
  }
}

object TwentyoneFifteen extends App with Support {
  @tailrec
  def findPath(q: mutable.PriorityQueue[Path], map: Map[Coord, Int], target: Coord, seen: Set[Coord]): Int = {
    val head = q.dequeue()
    if (head.position == target) head.risk
    else if (seen.contains(head.position)) findPath(q, map, target, seen)
    else {
      val nextToVisit = head.position.cardinalNeighbours
        .filter(_.inBounds(target))
        .filterNot(seen.contains)
        .map(loc => Path(loc, head.risk + map(loc)))
      q.enqueue(nextToVisit: _*)
      findPath(q, map, target, seen + head.position)
    }
  }

  val i = load2dIntSeqWithCoords(delimiterB = "")

  val target = i.keys.maxBy(_.manhattan(Coord(0, 0)))
  val size = target.x + 1

  val part1 = findPath(mutable.PriorityQueue(Path(Coord(0, 0), 0)), i, target, Set.empty)
  println(part1)

  val mapRepeated: Map[Coord, Int] = (for {
    rx <- 0 until 5
    ry <- 0 until 5
  } yield {
    i.toSeq.map { case (Coord(x, y), risk) => (Coord(x + (rx * size), y + (ry * size)), risk + rx + ry) }.map {
      case (loc, risk) if risk > 9 => (loc, (risk % 10) + 1)
      case otherwise               => otherwise
    }
  }).flatten.toMap

  val largerTarget = mapRepeated.keys.maxBy(_.manhattan(Coord(0, 0)))

  val part2 = findPath(mutable.PriorityQueue(Path(Coord(0, 0), 0)), mapRepeated, largerTarget, Set.empty)
  println(part2)
}
