import lib.{Coord, Support}

object d3 extends App with Support {
  private final case class PartNumber(n: Long, part: Coord)
  private final case class State(seen: List[PartNumber] = Nil, current: String = "", part: Option[Coord] = None)

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
    val gearLocs = parts.filter(_._2 == '*').map(_._1).toSet

    val numbers = in.zipWithIndex.flatMap { case (row, y) =>
      // go through all the numbers, building up iteratively into acc. as you go through each char, check if any of the
      // neighbouring locations has a part, if so store into valid. if valid is still false when you move to a
      // non-digit char, then this isn't a part number and can be discarded
      val State(rowNumbers, maybeFinalNo, valid) = row.zipWithIndex
        .map { case (c, x) => (Coord(x, y), c) }
        .foldLeft(State()) { case (State(seen, acc, validLoc), (loc, c)) =>
          if (c.isDigit) State(seen, acc + c, validLoc orElse partLocs.intersect(loc.neighbours.toSet).headOption)
          else if (acc.nonEmpty && validLoc.isDefined) State(seen :+ PartNumber(acc.toLong, validLoc.get), "", None)
          else State(seen, "", None)
        }
      val allRowNumbers =
        if (valid.isDefined) rowNumbers :+ PartNumber(maybeFinalNo.toLong, valid.get) else rowNumbers
      allRowNumbers
    }

    val numbersByPart = numbers.groupMap(_.part)(_.n)

    lazy val p1 = numbers.map(_.n).sum

    lazy val p2 = gearLocs.toList.map(loc => numbersByPart(loc)).filter(_.size == 2).map(_.product).sum

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
