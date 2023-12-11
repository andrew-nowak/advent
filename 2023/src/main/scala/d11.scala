import lib.{Coord, Support}

object d11 extends App with Support {
  val testData =
    """
      |...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....
      |""".trim.stripMargin
  val input = load

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = charCoords(data)

    val max = in.maxBy(c => c._1.x + c._1.y)._1

    val galaxies = in.filter(_._2 == '#').keys

    val emptyCols = (0 to max.x).filter(x => !galaxies.exists(_.x == x))
    val emptyRows = (0 to max.y).filter(y => !galaxies.exists(_.y == y))

    val p1 = {
      val colMap: Map[Int, Int] = (0 to max.x).map { n =>
        n -> (n + emptyCols.count(_ < n))
      }.toMap
      val rowMap: Map[Int, Int] = (0 to max.y).map { n =>
        n -> (n + emptyRows.count(_ < n))
      }.toMap

      val adjustedGalaxies = galaxies.map { case Coord(x, y) =>
        Coord(
          colMap(x),
          rowMap(y)
        )
      }.toList

      adjustedGalaxies
        .combinations(2)
        .map { case a :: b :: Nil => a.manhattan(b) }
        .sum
    }

    val p2 = {
      val scale = 1000_000
      val colMap: Map[Int, Int] = (0 to max.x).map { n =>
        n -> (n + emptyCols.count(_ < n) * (scale - 1))
      }.toMap
      val rowMap: Map[Int, Int] = (0 to max.y).map { n =>
        n -> (n + emptyRows.count(_ < n) * (scale - 1))
      }.toMap

      val adjustedGalaxies = galaxies.map { case Coord(x, y) =>
        Coord(
          colMap(x),
          rowMap(y)
        )
      }.toList

      adjustedGalaxies
        .combinations(2)
        .map { case a :: b :: Nil => a.manhattan(b).toLong }
        .sum
    }

    println(p1)

    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
