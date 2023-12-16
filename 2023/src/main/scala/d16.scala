import lib.Direction._
import lib.{Coord, Direction, Support}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object d16 extends App with Support {
  val testData =
    """
      |.|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin.trim
  val input = load

  private def visited(
      c: Coord,
      d: Direction,
      e: Map[Coord, Set[Direction]]
  ): Boolean =
    e.get(c).exists(_.contains(d))

  @tailrec private def energize(
      q: Queue[(Coord, Direction)],
      m: Map[Coord, Char],
      e: Map[Coord, Set[Direction]]
  ): Set[Coord] = {
    if (q.isEmpty) e.keySet
    else {
      val ((loc, dir), rest) = q.dequeue
      if (visited(loc, dir, e)) energize(rest, m, e)
      else {
        val newEnergizeds = e.updatedWith(loc) {
          case Some(dirs) => Some(dirs + dir)
          case None       => Some(Set(dir))
        }
        m.get(loc) match {
          case None => energize(rest, m, e)
          case Some(nextTile) =>
            val nextDirs: Set[Direction] = (nextTile, dir) match {
              case ('/', Up) | ('\\', Down)    => Set(Right)
              case ('/', Down) | ('\\', Up)    => Set(Left)
              case ('/', Left) | ('\\', Right) => Set(Down)
              case ('/', Right) | ('\\', Left) => Set(Up)
              case ('-', Up) | ('-', Down)     => Set(Left, Right)
              case ('|', Left) | ('|', Right)  => Set(Up, Down)
              case ('.', d)                    => Set(d)
              case ('-', d)                    => Set(d)
              case ('|', d)                    => Set(d)
            }
            val newStates = nextDirs.map(d => (loc.go(d), d))

            energize(rest.enqueueAll(newStates), m, newEnergizeds)
        }
      }
    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = charCoords(data)

    val p1 = energize(Queue((Coord(0, 0), Right)), in, Map.empty)

    val maxX = in.keySet.map(_.x).max
    val maxY = in.keySet.map(_.y).max

    val starts = {
      val roof = for { x <- 0 until maxX } yield (Coord(x, 0), Down)
      val floor = for { x <- 0 until maxX } yield (Coord(x, maxY), Up)
      val leftWall = for { y <- 0 until maxY } yield (Coord(0, y), Right)
      val rightWall = for { y <- 0 until maxY } yield (Coord(maxX, y), Left)
      roof ++ floor ++ leftWall ++ rightWall
    }
    val p2 = for { s <- starts } yield energize(Queue(s), in, Map.empty).size

    println(p1.size)

    println(p2.max)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)

}
