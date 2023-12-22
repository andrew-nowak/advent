import lib._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object d21 extends App with Support {

  val testData =
    """...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin.trim
  val input = load

  final case class Chessboard(
      allOdd: Int,
      allEven: Int,
      intOdd: Int,
      intEven: Int,
      extOdd: Int,
      extEven: Int
  )

  @tailrec def accessible(
      q: Set[Coord],
      rocks: Set[Coord],
      visited: Set[Coord],
      bound: Coord
  ): Set[Coord] = {
    if (q.isEmpty) visited
    else {
      val (h, t) = q.splitAt(1)
      if (visited contains h.head) accessible(t, rocks, visited, bound)
      else {
        val next = t ++ h.head.cardinalNeighbours.toSet
          .filter(_.inBounds(bound))
          .diff(rocks)
          .diff(visited)
        accessible(next, rocks, visited ++ h, bound)
      }
    }
  }

  @tailrec def chessboard(
      q: Queue[(Coord, Int)],
      rocks: Set[Coord],
      visited: Map[Coord, Int],
      tile: Set[Coord]
  ): Chessboard = {
    q.dequeueOption match {
      case None =>
        val even = visited.filter(_._2 % 2 == 0).keySet
        val odd = visited.filter(_._2 % 2 == 1).keySet
        val int = visited.filter(_._2 < 66).keySet
        val ext = visited.filter(_._2 >= 66).keySet
        println("inaccessible gardens", visited.find(_._1 == Coord(65, 65)))

        Chessboard(
          allOdd = odd.size,
          allEven = even.size,
          intOdd = (int & odd).size,
          intEven = (int & even).size,
          extOdd = (ext & odd).size,
          extEven = (ext & even).size
        )
      case Some(((c, n), t)) if visited contains c =>
        chessboard(t, rocks, visited, tile)
      case Some(((c, n), t)) =>
        val nbors = c.cardinalNeighbours.toSet
          .intersect(tile)
          .diff(rocks)
          .diff(visited.keySet)
        val nv = visited + ((c, n))
        val nq = t.enqueueAll(nbors.map((_, n + 1)))
        chessboard(nq, rocks, nv, tile)
    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = charCoords(data)
    val tileWidth = data.split("\n").length

    println("gardens:", in.count(_._2 == '.'))

    val startLoc = in.collectFirst { case (coord, 'S') => coord }.get
    println(startLoc)
    val map = in.updated(startLoc, '.')
    val rocks = map.collect { case (coord, '#') => coord }.toSet

    val bound = map.keySet.maxBy(_.manhattan(Origin))

    val accessibleGardens =
      accessible(Set(Coord(0, 0)), rocks, Set.empty, bound)
    val inaccessible = map.keySet.diff(rocks).diff(accessibleGardens)

    val near = accessibleGardens.filter(_.manhattan(startLoc) < 66)
    val far = accessibleGardens.diff(near)
    val even = accessibleGardens.filter(_.manhattan(startLoc) % 2 == 0)
    val odd = accessibleGardens.diff(even)
    val nearEven = (near & even).size
    val nearOdd = (near & odd).size
    val farOdd = (far & odd).size
    val farEven = (far & even).size

    val p1 = nearEven

//    val tileChessed = chessboard(Queue((startLoc, 0)), rocks, Map.empty, map.keySet)
    println(p1)

    val p2Steps = 26501365L
//    /*
//         ....#... 0 -> 1, 1 -> 5, 2 -> 13, 3 -> 25
//         ...###...
//         ..#####..
//         .#######.
//         ####S####
//         .#######.
//         ..#####..
//         ...###...
//         ....#....
//     */
//
    val radius = p2Steps / tileWidth

    println(p2Steps, tileWidth, radius, Seq(near, far, even, odd).map(_.size))
    println(farOdd, farEven, nearOdd, nearEven)
//    println(tileChessed)

    val w = radius - 1

//    val a = 2*w*w + 2*w + 1

//    val interior = a / 2 *even.size + ((a / 2) + 1)*odd.size
    val interior = even.size * (w + 1) * (w + 1) + odd.size * w * w

    val exterior =
      4 * radius * nearOdd + 3 * w * farOdd + radius * farEven + 2 * farOdd
    println(radius, farEven)

    println(interior + exterior)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
//  run(testData)
  println("--- real ---")
  run(input)
}
