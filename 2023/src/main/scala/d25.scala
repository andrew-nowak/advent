import lib._

import scala.annotation.tailrec

object d25 extends App with Support {

  val testData =
    """jqt: rhn xhk nvd
      |rsh: frs pzl lsr
      |xhk: hfx
      |cmg: qnr nvd lhk bvb
      |rhn: xhk bvb hfx
      |bvb: xhk hfx
      |pzl: lsr hfx nvd
      |qnr: nvd
      |ntq: jqt hfx bvb xhk
      |nvd: lhk
      |lsr: lhk
      |rzs: qnr cmg lsr rsh
      |frs: qnr lhk lsr""".stripMargin.trim
  val input = load

  @tailrec def tour(
      q: List[String],
      visited: Set[String],
      m: Map[String, Set[String]]
  ): Set[String] = {
    q match {
      case Nil => visited
      case h :: t =>
        val next = m(h) diff visited
        tour(t ++ next, visited + h, m)
    }
  }

  def run(data: String): Unit = {
    val startTime = System.nanoTime()

    val in = stringSeq(data)

    // found through visualizing with d3-force
    val rm = Set(
      "sfm" -> "vmt",
      "rmg" -> "fql",
      "vph" -> "mfc"
    )

    val links = in
      .flatMap { case s"${a}: ${rest}" =>
        rest.split(' ').flatMap(b => Seq(a -> b))
      }
      .toSet
      .diff(rm)

    val map = (links ++ links.map(_.swap)).groupMap(_._1)(_._2)

    val nodes = links.map(_._1) ++ links.map(_._2)

    val g1 = tour("sfm" :: Nil, Set.empty, map)
    val g2 = nodes -- g1
    val p1 = g1.size * g2.size
    println(p1)

//    val p2 = in.size
//    println(p2)

    val endTime = System.nanoTime()
    println(s"Done in ${(endTime - startTime).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
//  run(testData)
  println("--- real ---")
  run(input)
}
