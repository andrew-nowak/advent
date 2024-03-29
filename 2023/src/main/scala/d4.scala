import lib.{Coord, Support}

object d4 extends App with Support {
  private final case class PartNumber(n: Long, part: Coord)
  private final case class State(
      seen: List[PartNumber] = Nil,
      current: String = "",
      part: Option[Coord] = None
  )

  val cardR = """Card +(\d+): ([0-9 ]+) \| ([0-9 ]+)""".r

  val testData =
    """Card   1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card   2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card   3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card   4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card   5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card   6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
      |""".trim.stripMargin
  val input = load

  val scores = 0 #:: LazyList.iterate(1)(_ * 2)

  def run(data: String): Unit = {
    val in = stringSeq(data)

    lazy val p1 = {
      val cards = in.map { _card =>
        val card = _card.drop(10).split("\\|")
        val Array(winners, numbers) =
          card.map(_.split(" ").flatMap(_.toIntOption))

        scores(winners.toSet.intersect(numbers.toSet).size)
      }
      cards.sum
    }

    lazy val p2 = {
      val cards = in.map { case cardR(cardno, winnos, nos) =>
        val card = cardno.toInt
        val winners = winnos.split(" ").flatMap(_.toIntOption).toSet
        val numbers = nos.split(" ").flatMap(_.toIntOption).toSet
        (card, winners.intersect(numbers).size)
      }
      val initialCardCounts = cards.map(_._1 -> 1L).toMap

      cards
        .foldLeft(initialCardCounts)((cardCounts, card) => {
          val (cardno, wins) = card
          ((cardno + 1) to (cardno + wins)).foldLeft(cardCounts)((counts, n) =>
            counts.updatedWith(n) { case Some(y) =>
              Some(y + cardCounts(cardno))
            }
          )
        })
        .values
        .sum
    }

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
