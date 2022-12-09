package twentytwo.nine

import lib.{Coord, Support}
final case class State(head: Coord, tail: List[Coord], tailHistory: Set[Coord])

object TwentytwoNine extends App with Support {
  val testData =
    """
      |R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2
      |""".stripMargin.trim
  val input = load

  def clamp(n: Int, mn: Int = -1, mx: Int = 1): Int = {
    Math.max(Math.min(n, mx), mn)
  }
  def run(data: String, knots: Int) = {
    val in = stringSeq(data)
    val initialState = State(Coord(0, 0), List.fill(knots)(Coord(0, 0)), tailHistory = Set(Coord(0, 0)))

    val motions = in.flatMap { motion =>
      val Array(d, n) = motion.split(" ")
      Seq.fill(n.toInt)(d)
    }

    val positions = motions.foldLeft(initialState)((state, motion) => {
      val nextHead = motion match {
        case "U" => state.head.up
        case "D" => state.head.down
        case "L" => state.head.left
        case "R" => state.head.right
      }
      val nextTail = state.tail.foldLeft[List[Coord]](Nil)((acc, knot) => {
        val prev = if (acc.isEmpty) nextHead else acc.last
        val nextKnot = if (prev.surrounding contains knot) {
          knot
        } else {
          (prev, knot) match {
            case (Coord(hx, hy), Coord(tx, ty)) =>
              Coord(tx + clamp(hx - tx), ty + clamp(hy - ty))
          }
        }
        acc :+ nextKnot
      })

      State(nextHead, nextTail, state.tailHistory + nextTail.last)
    })

    println(positions.tailHistory.size)
  }

  println("--- testdata ---")
  run(testData, 1)
  run(testData, 9)
  println("--- real ---")
  run(input, 1)
  run(input, 9)
}
