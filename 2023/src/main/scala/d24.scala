import lib._

import scala.annotation.tailrec

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
//          println(px, py, m2, o.px, o.py, m, x, y) // 25.333, 14.667
          Some((x, y))
        } else None
      }
    }
  }

  case class C3(x: Double, y: Double, z: Double) {
    def mag: Double = math.sqrt(x * x + y * y + z * z)

    def +(o: C3): C3 = C3(x + o.x, y + o.y, z + o.z)
    def -(o: C3): C3 = C3(x - o.x, y - o.y, z - o.z)
    def *:(n: Double): C3 = C3(x * n, y * n, z * n)
    def =~=(o: C3, eps: Double = 0.00001): Boolean = {
      def almost(a: Double, b: Double): Boolean = (a - b).abs < eps
      almost(x, o.x) && almost(y, o.y) && almost(z, o.z)
    }

    def dot(o: C3): Double = x * o.x + y * o.y + z * o.z
  }
  private def dmnop(m: C3, n: C3, o: C3, p: C3): Double = {
    ((m.x - n.x) * (o.x - p.x)) + ((m.y - n.y) * (o.y - p.y)) + ((m.z - n.z) * (o.z - p.z))
  }

  case class Vec3(
      px: Double,
      py: Double,
      pz: Double,
      dx: Double,
      dy: Double,
      dz: Double
  ) {
    def dist2(o: Vec3): Double = {
      val norm = C3(
        x = (dy * o.dz) - (dz * o.dy),
        y = (dz * o.dx) - (dx * o.dz),
        z = (dx * o.dy) - (dy * o.dx)
      )
      val p1 = C3(px, py, pz)
      val p2 = C3(o.px, o.py, o.pz)
      val nom = (norm dot (p2 - p1)).abs
      val den = norm.mag
      nom / den
    }
    // https://paulbourke.net/geometry/pointlineplane/#:~:text=The%20shortest%20line%20between%20two%20lines%20in%203D
    def dist(o: Vec3): Double = {
      val p1: C3 = C3(px, py, pz)
      val p2: C3 = C3(px + dx, py + dy, pz + dz)
      val p3: C3 = C3(o.px, o.py, o.pz)
      val p4: C3 = C3(o.px + o.dx, o.py + o.dy, o.pz + o.dz)

      val mua = ((dmnop(p1, p3, p4, p3) * dmnop(p4, p3, p2, p1)) - (dmnop(
        p1,
        p3,
        p2,
        p1
      ) * dmnop(p4, p3, p4, p3))).toDouble /
        ((dmnop(p2, p1, p2, p1) * dmnop(p4, p3, p4, p3)) - (dmnop(
          p4,
          p3,
          p2,
          p1
        ) * dmnop(p4, p3, p2, p1)))

      val mub = (dmnop(p1, p3, p4, p3) + (mua * dmnop(p4, p3, p2, p1))) / dmnop(
        p4,
        p3,
        p4,
        p3
      )

      val pa = p1 + (mua *: (p2 - p1))
      val pb = p3 + (mub *: (p4 - p3))

      (pa - pb).mag
//      if (pa =~= pb) Some(pa)
//      else None
    }

  }

  def run(data: String, cmin: Long, cmax: Long): Unit = {
    val startTime = System.nanoTime()

    val in = stringSeq(data)

    val vs = in.map { case s"${x}, ${y}, ${_} @ ${dx}, ${dy}, ${_}" =>
      Vec(x.trim.toLong, y.trim.toLong, dx.trim.toLong, dy.trim.toLong)
    }

    val paired = vs.combinations(2).map { case Seq(a, b) => (a, b) }.toSeq

    val p1 = paired.count { case (a, b) =>
      a intersects b match {
        case None => false
        case Some((x, y)) =>
          x >= cmin && x <= cmax && y >= cmin && y <= cmax
      }
    }
    println(p1)

    val v3s = in.map { case s"${x}, ${y}, ${z} @ ${dx}, ${dy}, ${dz}" =>
      Vec3(
        x.trim.toDouble,
        y.trim.toDouble,
        z.trim.toDouble,
        dx.trim.toDouble,
        dy.trim.toDouble,
        dz.trim.toDouble
      )
    } // ++ Seq(Vec3(24D, 13D, 10D, -3D, 1D, 2D))

    val (testVecs, Seq(one, two)) = v3s.splitAt(v3s.size - 2)

    @tailrec def findTimes(
        v1: Vec3,
        v2: Vec3,
        tvs: List[Vec3],
        amin: Long,
        amax: Long,
        bmin: Long,
        bmax: Long,
        bestv: Vec3
    ): (Long, Long, Vec3) = {
      if (amin == amax && bmin == bmax) (amin, bmin, bestv)
      else {
        println("prms", amin, amax, bmin, bmax)
        val da = (amax - amin) / 20L
        val db = (bmax - bmin) / 20L
        val as = amin.to(math.max(amin, amax), math.max(1, da)) :+ amax
        val bs = bmin.to(math.max(bmin, bmax), math.max(1, db)) :+ bmax
        val opts = for {
          a1 <- as
//          a = ra + (da / 2)
          b1 <- bs
//          b1 = rb + (db / 2)
          (a, b) <-
            if (a1 == b1) Seq((a1, b1 + 1), (a1 + 1, b1)) else Seq((a1, b1))
//          _ = if (b==a) println("ba", b,a)
//          if b != a
          dx = ((v2.px + b * v2.dx) - (v1.px + a * v1.dx)) / (b - a)
          x = v1.px + a * (v1.dx - dx)
          dy = ((v2.py + b * v2.dy) - (v1.py + a * v1.dy)) / (b - a)
          y = v1.py + a * (v1.dy - dy)
          dz = ((v2.pz + b * v2.dz) - (v1.pz + a * v1.dz)) / (b - a)
          z = v1.pz + a * (v1.dz - dz)
          v = Vec3(x, y, z, dx, dy, dz)
        } yield {
          val err = tvs.map(v.dist2).sum
//          println(a, b, err)
          (a, b, err.abs, v)
        }
        val best = opts.minBy(_._3)
        println("best", best)
//        for { o <- opts } { println(o) }
        println("0", opts.head)
        findTimes(
          v1,
          v2,
          v3s.toList,
          math.max(0, best._1 - da),
          best._1 + da,
          math.max(0, best._2 - db),
          best._2 + db,
          best._4
        )
      }
    }

    // 1_000_000_000_000_000_000L
    val (a, b, v) = findTimes(
      one,
      two,
      testVecs.toList.take(1),
      1L,
      1_000_000_000_000_000_000L,
      1L,
      1_000_000_000_000_000_000L,
      one
    )
    println(v)

    val ans = v.px + v.py + v.pz

    println(ans.round)

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

//777844757407882 -- too low
//6495466595465981 -- too high
//852912669337968 -- too low
//852912669337969 -- nope
//1033770143421619 -- yep
