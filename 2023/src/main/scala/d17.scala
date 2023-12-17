import lib.Direction.{Down, Right}
import lib.{Coord, Direction, Support}

import scala.annotation.tailrec

object d17 extends App with Support {
  val testData =
    """
      |2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin.trim
  val input = load

  final case class State(
      pos: Coord,
      dir: Direction,
      times: Int,
      heatLoss: Int
  ) {
    val visit: (Coord, Direction, Int) = (pos, dir, times)
  }

  @tailrec def astarStandard(
      q: List[State],
      m: Map[Coord, Int],
      dest: Coord,
      visited: Set[(Coord, Direction, Int)]
  ): Int = {
    q match {
      case Nil                           => -1
      case head :: _ if head.pos == dest => head.heatLoss
      case head :: rest if visited contains head.visit =>
        astarStandard(rest, m, dest, visited)
      case s :: rest =>
        val nextDs =
          Direction.all - s.dir.rev diff (if (s.times == 3) Set(s.dir)
                                          else Set.empty)
        val newStates = nextDs.toSeq.map(d => d -> s.pos.go(d)).collect {
          case (d, pos) if pos.inBounds(dest) =>
            State(
              pos,
              d,
              if (d == s.dir) s.times + 1 else 1,
              s.heatLoss + m(pos)
            )
        }

        val nq =
          (rest ++ newStates).sortBy(s => s.pos.manhattan(dest) + s.heatLoss)

        astarStandard(nq, m, dest, visited + s.visit)
    }
  }

  @tailrec def astarUltra(
      q: List[State],
      m: Map[Coord, Int],
      dest: Coord,
      visited: Set[(Coord, Direction, Int)]
  ): Int = {
    q match {
      case Nil                                              => -1
      case head :: _ if head.pos == dest && head.times >= 4 => head.heatLoss
      case head :: rest if visited contains head.visit =>
        astarUltra(rest, m, dest, visited)
      case s :: rest =>
        val nextDs =
          if (s.times < 4) Set(s.dir)
          else
            Direction.all - s.dir.rev diff (if (s.times == 10) Set(s.dir)
                                            else Set.empty)
        val newStates = nextDs.toSeq.map(d => d -> s.pos.go(d)).collect {
          case (d, pos) if pos.inBounds(dest) =>
            State(
              pos,
              d,
              if (d == s.dir) s.times + 1 else 1,
              s.heatLoss + m(pos)
            )
        }

        val nq =
          (rest ++ newStates).sortBy(s => s.pos.manhattan(dest) + s.heatLoss)

        astarUltra(nq, m, dest, visited + s.visit)
    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = `2dIntSeqWithCoords`(data, delimiterB = "")

    val maxX = in.map(_._1.x).max
    val maxY = in.map(_._1.y).max
    val dest = Coord(maxX, maxY)

    val initialStates = List(Down, Right).map(d => State(Coord(0, 0), d, 1, 0))
    val p1 = astarStandard(initialStates, in, dest, Set.empty)
    println(p1)

    val p2 = astarUltra(initialStates, in, dest, Set.empty)
    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)

}
