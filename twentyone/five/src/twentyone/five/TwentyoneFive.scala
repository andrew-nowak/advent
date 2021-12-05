package twentyone.five

import lib.Support

final case class Coord(x: Int, y: Int)

object TwentyoneFive extends App with Support {
  val pat = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
  def parse(raw: Seq[String]): Seq[(Coord, Coord)] = raw map { case pat(ax, ay, bx, by) =>
    (Coord(ax.toInt, ay.toInt), Coord(bx.toInt, by.toInt))
  }

  def unwindVh(line: (Coord, Coord)): Seq[Coord] = {
    val dx = if (line._1.x > line._2.x) -1 else 1
    val dy = if (line._1.y > line._2.y) -1 else 1
    for {
      x <- line._1.x.to(line._2.x, dx)
      y <- line._1.y.to(line._2.y, dy)
    } yield Coord(x, y)
  }
  def unwindDiag(line: (Coord, Coord)): Seq[Coord] = {
    val dx = if (line._1.x > line._2.x) -1 else 1
    val dy = if (line._1.y > line._2.y) -1 else 1
    for {
      (x, y) <- line._1.x.to(line._2.x, dx).zip(line._1.y.to(line._2.y, dy))
    } yield Coord(x, y)
  }

  val i = loadStringSeq
  val p = parse(i)
  val (vh, diag) = p.partition(line => line._1.x == line._2.x || line._1.y == line._2.y)

  val locsVh = vh.flatMap(unwindVh)
  val locsDiag = diag.flatMap(unwindDiag)

  val part1 = locsVh.groupBy(coord => coord).count(_._2.size > 1)
  println(part1)

  val part2 = (locsVh ++ locsDiag).groupBy(coord => coord).count(_._2.size > 1)
  println(part2)
}
