import lib._

object d24 extends App with Support {

  val testData =
    """19, 13, 30 @ -2,  1, -2
      |18, 19, 22 @ -1, -1, -2
      |20, 25, 34 @ -2, -2, -4
      |12, 31, 28 @ -1, -2, -1
      |20, 19, 15 @  1, -5, -3""".stripMargin.trim
  val input = load

  case class Vec(px: Long, py: Long, vx: Long, vy: Long) {
    def intersects(o: Vec): Option[(Double, Double)] = {
      val det = (vx * -o.vy) - (-o.vx * vy)
//      println("--------")
//      println(this, o)
//      println("det", det)
      if (det == 0) None
      else {
        val m = (1d / det.toDouble) * ((-vy * (o.px - px)) + (vx * (o.py - py)))
        val m2 =
          (1d / det.toDouble) * ((-o.vy * (o.px - px)) + (o.vx * (o.py - py)))
        val x = o.px + (m * o.vx)
        val y = o.py + (m * o.vy)
        if (m >= 0 && m2 >= 0) {
          Some((x, y))
        } else None
      }
    }
  }

  def run(data: String, cmin: Long, cmax: Long): Unit = {
    val startTime = System.nanoTime()

    val in = stringSeq(data)

    val vs = in.map { case s"${x}, ${y}, ${_} @ ${dx}, ${dy}, ${_}" =>
      Vec(x.trim.toLong, y.trim.toLong, dx.trim.toLong, dy.trim.toLong)
    }

    val paired = vs.combinations(2).map { case Seq(a, b) => (a, b) }

    val p1 = paired.count { case (a, b) =>
      a intersects b match {
        case None => false
        case Some((x, y)) =>
          x >= cmin && x <= cmax && y >= cmin && y <= cmax
      }
    }
    println(p1)

    val p2 = in.size
//    println(p2)

    val endTime = System.nanoTime()
    println(s"Done in ${(endTime - startTime).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData, 7L, 27L)
  println("--- real ---")
  run(input, 200000000000000L, 400000000000000L)
}
