package nineteen.ten

import lib.{Coord, Support}

object NineteenTen extends App with Support {
  val testData =
    """
      |.#..##.###...#######
      |##.############..##.
      |.#.######.########.#
      |.###.#######.####.#.
      |#####.##.#.##.###.##
      |..#####..#.#########
      |####################
      |#.####....###.#.#.##
      |##.#################
      |#####.##.###..####..
      |..######..##.#######
      |####.##.####...##..#
      |.#####..#.######.###
      |##...#.##########...
      |#.##########.#######
      |.####.#.###.###.#.##
      |....##.##.###..#####
      |.#.#.###########.###
      |#.#.#.#####.####.###
      |###.##.####.##.#..##
      |""".stripMargin.trim
  val input = load

  def run(data: String) = {
    val in = stringSeq(data)

    val map = in.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.collect { case (c, x) if c == '#' => Coord(x, y) }
    }.toSet

    def detections(c: Coord) = {
      val candidates = map - c
      val normed = candidates
        .groupBy(cand => {
          val dx = cand.x - c.x
          val dy = cand.y - c.y
          val g = Math.abs(gcd(dx, dy))
          Coord(dx / g, dy / g)
        })
        .view
        .mapValues(_.toSeq.sortBy(_.manhattan(Coord(0, 0))))
        .toMap
      c -> normed
    }

    def transpose[A](xs: Seq[Seq[A]]): Seq[Seq[A]] = xs.filter(_.nonEmpty) match {
      case Nil => Nil
      case ys  => ys.map(_.head) +: transpose(ys.map(_.tail))
    }

    val p1 = map.map(detections).maxBy(_._2.size)

    println(p1._2.size)

//    println(p1._2)

    val ordered = p1._2
      .map { case (k, v) =>
        Math.atan2(k.x.toDouble, k.y.toDouble) -> v
      }
      .toSeq
      .sortBy(_._1)
      .map(_._2.sortBy(_.manhattan(p1._1)))
      .reverse
    val p2 = transpose(ordered).flatten.apply(200 - 1)
    println(p2.x * 100 + p2.y)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
