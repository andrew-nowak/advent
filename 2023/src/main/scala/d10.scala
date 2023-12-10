import lib.{Coord, Support}

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object d10 extends App with Support {
  val testData =
    """
      |..........
      |.S------7.
      |.|F----7|.
      |.||....||.
      |.||....||.
      |.|L-7F-J|.
      |.|..||..|.
      |.L--JL--J.
      |..........
      |""".trim.stripMargin
  val input = load

  private def loop(map: Map[Coord, Char], start: Coord): Set[Coord] =
    loop(map, Set.empty, start, 0L)

  @tailrec private def loop(
      map: Map[Coord, Char],
      visited: Set[Coord],
      at: Coord,
      steps: Long
  ): Set[Coord] = {
    (map(at) match {
      case '|' => Set(at.north, at.south)
      case '-' => Set(at.west, at.east)
      case 'L' => Set(at.north, at.east)
      case 'J' => Set(at.north, at.west)
      case '7' => Set(at.south, at.west)
      case 'F' => Set(at.south, at.east)
    }).diff(visited).headOption match {
      case None       => visited + at
      case Some(next) => loop(map, visited + at, next, steps + 1L)
    }
  }

  def run(data: String, sIs: Char): Unit = {
    val startTime = System.nanoTime()

    val (map, start) = {
      val coords = charCoords(data)
      val S = coords.find(_._2 == 'S').get._1
      (coords.updated(S, sIs), S)
    }

    val theLoop = loop(map, start)

    lazy val p1 = theLoop.size / 2

    val twists = Seq(Set('F', 'J'), Set('7', 'L'))

    lazy val p2 = {
      val space = map.keySet.diff(theLoop)

      space.filter(location => {
        // how to know if you are "inside" the pipe?
        // to be inside, there must be a non-even number of pipe "stretches" in each cardinal direction
        // what is a pipe stretch? each perpendicular char (eg. '|' when looking west) is 1 stretch,
        // but there can be multiple parallel chars (eg. '----' would be one stretch)
        // parallel stretches must be formed of two turns with any number of straights inside, e.g 'F----J',
        // so we can ignore the parallel straights, and just count the pairs of turns.
        // but not all turn pairs count! a stretch like 'L----J' doesn't count because if that's all between
        // you and the outside, you're still outside! only 'FJ' and 'L7' pairs count, because they're
        // twists, rather than U-bends.
        // also, you only need to know about north and west - because pipes can't cross, you know that if
        // you appear to be inside by looking west, then you'll also appear inside when looking east
        // (the number of stretches of pipe on any row or column MUST be even, so if there's an odd number
        // west then there'll be an odd number east, etc.etc.)

        val (leftStraight, leftTurn) = (0 until location.x)
          .map(Coord(_, location.y))
          .filter(theLoop.contains)
          .map(map)
          .filterNot(_ == '.')
          .filterNot(_ == '-')
          .partition(_ == '|')

        val left = leftStraight.size + leftTurn
          .grouped(2)
          .map(_.toSet)
          .count(twists.contains)

        val (upStraight, upTurn) = (0 until location.y)
          .map(Coord(location.x, _))
          .filter(theLoop.contains)
          .map(map)
          .filterNot(_ == '.')
          .filterNot(_ == '|')
          .partition(_ == '-')
        val up =
          upStraight.size + upTurn
            .grouped(2)
            .map(_.toSet)
            .count(twists.contains)

        Seq(left, up).forall(n => n % 2 != 0)
      })
    }

    val dbgmap = map.filter(theLoop contains _._1) ++ p2.map(c => c -> 'X')

    val pretty = showCoords[Char](
      dbgmap,
      {
        case '-' => '─'
        case '|' => '│'
        case 'F' => '┌'
        case '7' => '┐'
        case 'L' => '└'
        case 'J' => '┘'
        case 'X' => 'X'
      },
      default = '?'
    )

    Files.writeString(Path.of("dbg.txt"), pretty)

    println(p1)

    println(p2.size)

    val end = System.nanoTime()
    println(s"Done in ${(end - startTime).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData, 'F')
  println("--- real ---")
  run(input, '|')
}
