import lib.Support

object d7 extends App with Support {

  val testData =
    """
      |32T3K 765
      |T55J5 684
      |KK677 28
      |KTJJT 220
      |QQQJA 483
      |""".trim.stripMargin
  val input = load

  def getType(hand: String): Int = {
    val cs = hand.toList.groupBy(identity).map { case (c, cs) => (c, cs.size) }
    if (cs.size == 1) { // five of kind
      1
    } else if (cs.values.exists(_ == 4)) { // four of kind
      2
    } else if (cs.values.toSet == Set(3, 2)) { // full house
      3
    } else if (cs.values.exists(_ == 3)) { // three of kind
      4
    } else if (cs.values.count(_ == 2) == 2) { // two pair
      5
    } else if (cs.values.exists(_ == 2)) { // one pair
      6
    } else { // high card
      7
    }
  }

  def getJokerAwareType(hand: String): Int = {
    val csj = hand.toList.groupBy(identity).map { case (c, cs) => (c, cs.size) }
    val jokers = csj.getOrElse('J', 0)
    val cs = csj.removed('J')
    if (jokers == 0) {
      getType(hand)
    } else if (cs.size == 1 || jokers == 5) { // five of kind
      1
    } else if (cs.values.exists(_ + jokers == 4)) { // four of kind
      2
    } else if (cs.values.toList == List(2, 2) && jokers == 1) { // full house
      3
    } else if (cs.values.exists(_ + jokers == 3)) { // three of kind
      4
    } else if (false) { // two pair - not possible with a joker
      5
    } else if (cs.values.exists(_ + jokers == 2)) { // one pair
      6
    } else {
      throw new IllegalArgumentException(hand)
    }
  }

  val strength = List('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4',
    '3', '2').zipWithIndex.toMap

  def run(data: String): Unit = {
    val in = stringSeq(data)

    val handsAndBids = in.map(_.split(" ")).map {
      case Array(hand, bid) => (hand, bid.toLong)
      case o =>
        println(o.mkString("Array(", ",", ")"))
        throw new IllegalArgumentException("boo")
    }

    val ordered = handsAndBids.sortWith { case ((handA, _), (handB, _)) =>
      val typeA = getType(handA)
      val typeB = getType(handB)
      if (typeA != typeB) typeA > typeB
      else {
        val Some((cardA, cardB)) = handA.zip(handB).find(hs => hs._1 != hs._2)
        strength(cardA) > strength(cardB)
      }
    }

    lazy val p1 = ordered.zipWithIndex.map { case ((_, bid), i) =>
      bid * (i + 1)
    }.sum

    val strengthP2 = strength.updated('J', 20)

    val orderedP2 = handsAndBids.sortWith { case ((handA, _), (handB, _)) =>
      val typeA = getJokerAwareType(handA)
      val typeB = getJokerAwareType(handB)
      if (typeA != typeB) typeA > typeB
      else {
        val Some((cardA, cardB)) = handA.zip(handB).find(hs => hs._1 != hs._2)
        strengthP2(cardA) > strengthP2(cardB)
      }
    }

    lazy val p2 = orderedP2.zipWithIndex.map { case ((_, bid), i) =>
      bid * (i + 1)
    }.sum

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
