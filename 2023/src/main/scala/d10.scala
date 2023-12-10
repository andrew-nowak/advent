import lib.{Coord, Support}

import scala.annotation.tailrec

object d10 extends App with Support {
  val testData =
    """
      |..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...
      |""".trim.stripMargin
  val input = load

  def loopLength(map: Map[Coord, Char], sIs: Char): Long = {
    val S = map.find(_._2 == 'S').get._1
    loopLength(map.updated(S, sIs), Set.empty, S, 0L)
  }
  @tailrec def loopLength(
      map: Map[Coord, Char],
      visited: Set[Coord],
      at: Coord,
      steps: Long
  ): Long = {
    (map(at) match {
      case '|' => Set(at.north, at.south)
      case '-' => Set(at.west, at.east)
      case 'L' => Set(at.north, at.east)
      case 'J' => Set(at.north, at.west)
      case '7' => Set(at.south, at.west)
      case 'F' => Set(at.south, at.east)
    }).diff(visited).headOption match {
      case None       => steps + 1
      case Some(next) => loopLength(map, visited + at, next, steps + 1L)
    }
  }

  def run(data: String, sIs: Char): Unit = {
    val start = System.nanoTime()

    val map = charCoords(data)

    lazy val p1 = loopLength(map, sIs) / 2

    lazy val p2 = map.size

    println(p1)

    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData, 'F')
  println("--- real ---")
  run(input, '|')
}
