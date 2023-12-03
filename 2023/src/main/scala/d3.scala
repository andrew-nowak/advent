import lib.{Coord, Support}

object d3 extends App with Support {

  val testData =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..
      |""".trim.stripMargin
  val input = load

  def run(data: String): Unit = {
    val in = stringSeq(data)

    val parts = in.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.flatMap { case (c, x) =>
        if (!(c.isLetterOrDigit || c == '.')) Some(Coord(x, y), c) else None
      }
    }
    val partLocs = parts.map(_._1).toSet

    val numbers = in.zipWithIndex.flatMap { case (row, y) =>
      // go through all the numbers, building up iteratively into acc. as you go through each char, check if any of the
      // neighbouring locations has a part, if so store into valid. if valid is still false when you move to a
      // non-digit char, then this isn't a part number and can be discarded
      val (rowNumbers, maybeFinalNo, valid) = row.zipWithIndex
        .map { case (c, x) => (Coord(x, y), c) }
        .foldLeft((List.empty[String], "", false)) { case ((seen, acc, validLoc), (loc, c)) =>
          if (c.isDigit) (seen, acc + c, validLoc || partLocs.intersect(loc.neighbours.toSet).nonEmpty)
          else if (acc.nonEmpty && validLoc) (seen :+ acc, "", false)
          else (seen, "", false)
        }
      val allRowNumbers = (if (valid) rowNumbers :+ maybeFinalNo else rowNumbers).map(_.toLong)
      allRowNumbers
    }

    lazy val p1 = numbers.sum

    lazy val p2 = in.size

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
