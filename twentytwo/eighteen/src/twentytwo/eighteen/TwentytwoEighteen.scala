package twentytwo.eighteen

import lib.{Coord3d, Support}

import scala.annotation.tailrec

object TwentytwoEighteen extends App with Support {
  val testData =
    """
      |2,2,2
      |1,2,2
      |3,2,2
      |2,1,2
      |2,3,2
      |2,2,1
      |2,2,3
      |2,2,4
      |2,2,6
      |1,2,5
      |3,2,5
      |2,1,5
      |2,3,5
      |""".stripMargin.trim
  val input = load

  def countExposed(cubes: Set[Coord3d]): Coord3d => Int = { case c @ Coord3d(x, y, z) =>
    6 - (cubes - c).count { case Coord3d(ox, oy, oz) => (x - ox).abs + (y - oy).abs + (z - oz).abs == 1 }
  }

  def run(data: String) = {
    val in = `2dIntSeq`(data, delimiterB = ",").map { case Seq(x, y, z) => Coord3d(x, y, z) }.toSet

    val p1 = in.toSeq map countExposed(in)

    println(p1.sum)

    def inBounds(coord3d: Coord3d): Boolean = {
      val Coord3d(x, y, z) = coord3d
      x > -3 && x < 25 && y > -3 && y < 25 && z > -3 && z < 25
    }

    def inTightBounds(coord3d: Coord3d): Boolean = {
      val Coord3d(x, y, z) = coord3d
      x > -2 && x < 24 && y > -2 && y < 24 && z > -2 && z < 24
    }

    @tailrec def paint(q: List[Coord3d], painted: Set[Coord3d]): Set[Coord3d] = {
      if (q.isEmpty) painted
      else {
        val news = q.head.cardinalNeighbours.filter(inBounds).filterNot(in.contains).filterNot(painted.contains).toList
        paint(news ++ q.tail, painted ++ news)
      }
    }

    val air = paint(List(Coord3d(24, 24, 24)), Set.empty)

    val p2 = air.filter(inTightBounds).toSeq map countExposed(air)

    println(p2.sum)
//    val p2 = in.toSeq.map { case c@Coord3d(x, y, z) =>
////      println(c)
//      (in - c)//.filter { case Coord3d(ox, oy, oz) => (x - ox).abs + (y - oy).abs + (z - oz).abs == 1 }
//        .count {
//          case Coord3d(ox, oy, oz) if oy == y && oz == z && ox < x => in.exists(cand => cand.x < ox && cand.y == oy && cand.z == oz)
//          case Coord3d(ox, oy, oz) if oy == y && oz == z && ox > x => in.exists(cand => cand.x > ox && cand.y == oy && cand.z == oz)
//          case Coord3d(ox, oy, oz) if oy < y && oz == z && ox == x => in.exists(cand => cand.x == ox && cand.y < oy && cand.z == oz)
//          case Coord3d(ox, oy, oz) if oy > y && oz == z && ox == x => in.exists(cand => cand.x == ox && cand.y > oy && cand.z == oz)
//          case Coord3d(ox, oy, oz) if oy == y && oz < z && ox == x => in.exists(cand => cand.x == ox && cand.y == oy && cand.z < oz)
//          case Coord3d(ox, oy, oz) if oy == y && oz > z && ox == x => in.exists(cand => cand.x == ox && cand.y == oy && cand.z > oz)
//          case _ => false
//        }
//    }
//
//    // val p2 = in.last
//     println(p2.sum)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
