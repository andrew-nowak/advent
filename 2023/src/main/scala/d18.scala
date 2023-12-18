import lib.Direction._
import lib.{Coord, Direction, SeqExtras, Support}

import scala.annotation.tailrec

object d18 extends App with Support {
  val testData =
    """
      |R 6 (#70c710)
      |D 5 (#0dc571)
      |L 2 (#5713f0)
      |D 2 (#d2c081)
      |R 2 (#59c680)
      |D 2 (#411b91)
      |L 5 (#8ceee2)
      |U 2 (#caa173)
      |L 1 (#1b58a2)
      |U 2 (#caa171)
      |R 2 (#7807d2)
      |U 3 (#a77fa3)
      |L 2 (#015232)
      |U 2 (#7a21e3)""".stripMargin.trim
  val input = load

  @tailrec def flood(q: Set[Coord], boundary: Set[Coord], fill: Set[Coord]): Int = {
    if (q.isEmpty) fill.size
    else {
      val (head, tail) = q.splitAt(1)
      val nbors = (head.head.cardinalNeighbours.toSet diff boundary) diff fill
      flood(tail ++ nbors, boundary, fill + head.head)
    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data)

    val p1r = """([LRUD]) (\d+) .*""".r

    val insts: Seq[(Direction, Int)] = in.map {
      case p1r("L", n) => (Left, n.toInt)
      case p1r("R", n) => (Right, n.toInt)
      case p1r("U", n) => (Up, n.toInt)
      case p1r("D", n) => (Down, n.toInt)
    }

    val trench = insts.foldLeft(Seq(Coord(0, 0))) { case (acc@last :: _, (d, n)) =>
      SeqExtras.produce(1 to n)(last)((prev, _) => prev.go(d)).reverse ++ acc
    }.toSet

    val startLoc = {
      val minX = trench.map(_.x).min
      val minY = trench.map(_.y).min
      val tl = Coord(minX, minY)
      val min = trench.minBy(_.manhattan(tl))
      Coord(min.x + 1, min.y + 1)
    }
    printCoords(trench)
    println(startLoc)

    val p1 = flood(Set(startLoc), trench, Set.empty)
    println(p1 + trench.size)

    val p2 = in.size
    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)

}
