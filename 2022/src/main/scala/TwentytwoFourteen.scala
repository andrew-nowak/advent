package twentytwo.fourteen

import lib.{Coord, Support}

import scala.annotation.tailrec

object TwentytwoFourteen extends App with Support {
  val testData =
    """
      |498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9
      |""".stripMargin.trim
  val input = load

  def orderlessTo(a: Int, b: Int): Range = Math.min(a, b) to Math.max(a, b)

  def run(data: String) = {
    val in = stringSeq(data)

    val rock: Set[Coord] = in
      .flatMap(path => {
        path
          .split(" -> ")
          .map(coord => coord.split(',').map(_.toInt))
          .map(coord => Coord(coord.head, coord.last))
          .sliding(2)
          .flatMap { case Array(first, last) =>
            val Coord(firstX, firstY) = first
            val Coord(lastX, lastY) = last
            for {
              x <- orderlessTo(firstX, lastX)
              y <- orderlessTo(firstY, lastY)
            } yield Coord(x, y)
          }
      })
      .toSet

    val source = Coord(500, 0)

    val maxY = rock.map(_.y).max

    @tailrec
    def find(env: Set[Coord], at: Coord, p2: Boolean): Option[Coord] = {
      if (env contains at) Some(at)
      else if (p2 && at.y == maxY + 2) Some(at)
      else if (!p2 && at.y >= maxY) None
      else find(env, at.copy(y = at.y + 1), p2)
    }
    @tailrec
    def drop(env: Set[Coord], from: Coord = source, sandCount: Int, p2: Boolean): (Set[Coord], Int) = {
      find(env, from, p2) match {
        case None                           => (env, sandCount)
        case Some(found) if found == source => (env, sandCount)
        case Some(firstBelow) =>
          if (!(firstBelow.y == maxY + 2 || env.contains(firstBelow.left)))
            drop(env, firstBelow.left, sandCount, p2)
          else if (!(firstBelow.y == maxY + 2 || env.contains(firstBelow.right)))
            drop(env, firstBelow.right, sandCount, p2)
          else
            drop(env + firstBelow.copy(y = firstBelow.y - 1), source, sandCount + 1, p2)
      }
    }

    val p1 = drop(rock, source, 0, p2 = false)
    println(p1._2)

    val p2 = drop(rock, source, 0, p2 = true)
    println(p2._2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
