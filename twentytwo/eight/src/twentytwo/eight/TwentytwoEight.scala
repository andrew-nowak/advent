package twentytwo.eight

import lib.Support
import lib.Coord

object TwentytwoEight extends App with Support {
  val testData =
    """
      |30373
      |25512
      |65332
      |33549
      |35390
      |""".stripMargin.trim
  val input = load

  def countTrees(ts: Seq[(Coord, Int)]): Set[Coord] =
    ts.foldLeft((-1, Set.empty[Coord])){
      case ((highest, seen), tree) if tree._2 > highest => (tree._2, seen + tree._1)
      case (acc, _) => acc
    }._2

  def run(data: String) = {
    val in = `2dIntSeqWithCoords`(data, delimiterB = "")

    val rows = in.groupBy(_._1.y).values.map(_.toSeq.sortBy(_._1.x)).toSeq.sortBy(_.head._1.y)
    val cols = in.groupBy(_._1.x).values.map(_.toSeq.sortBy(_._1.y)).toSeq.sortBy(_.head._1.x)

    val p1 = {
      val left = rows.map(countTrees)
      val right = rows.map(_.reverse).map(countTrees)
      val top = cols.map(countTrees)
      val bot = cols.map(_.reverse).map(countTrees)
      left ++ right ++ top ++ bot
    }
    println(p1.flatten.toSet.size)

    val p2 = in.map { case (coord, height) =>
      val row = rows(coord.y)
      val col = cols(coord.x)
      val (left, right) = row.filterNot(_._1 == coord).splitAt(coord.x)
      val (top, bot) = col.filterNot(_._1 == coord).splitAt(coord.y)

      def count(s: Seq[(Coord, Int)]): Int = {
        val (bef, aft) = s.span(_._2 < height)
        bef.size + (if (aft.nonEmpty) 1 else 0)
      }

      val a = count(left.reverse)
      val b = count(right)
      val c = count(top.reverse)
      val d = count(bot)

      a * b * c * d
    }.max
    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
