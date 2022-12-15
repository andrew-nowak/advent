package twentytwo.thirteen

import lib.Support

sealed trait Token
case class I(value: Int) extends Token
case class L(value: List[Token]) extends Token

object TwentytwoThirteen extends App with Support {
  val testData =
    """
      |[1,1,3,1,1]
      |[1,1,5,1,1]
      |
      |[[1],[2,3,4]]
      |[[1],4]
      |
      |[9]
      |[[8,7,6]]
      |
      |[[4,4],4,4]
      |[[4,4],4,4,4]
      |
      |[7,7,7,7]
      |[7,7,7]
      |
      |[]
      |[3]
      |
      |[[[]]]
      |[[]]
      |
      |[1,[2,[3,[4,[5,6,7]]]],8,9]
      |[1,[2,[3,[4,[5,6,0]]]],8,9]
      |""".stripMargin.trim
  val input = load

  def parse(v: String): Token = {
    if (v.head == '[') parseList(v.tail)._1
    else I(v.toInt)
  }

  def parseList(v: String, current: List[Token] = Nil): (L, String) = {
    val u = v.dropWhile(_ == ',')

    if (u.isEmpty) (L(current), "")
    else if (u.head == '[') {
      val (innerList, rem) = parseList(u.tail)
      parseList(rem, current :+ innerList)
    } else if (u.head == ']') (L(current), u.tail)
    else {
      val (n, rem) = u.span(_.isDigit)
      parseList(rem, current :+ I(n.toInt))
    }
  }

  def compare(candidates: (Token, Token)): Option[Boolean] = {
    candidates match {
      case (I(x), I(y)) if x != y => Some(x < y)
      case (L(ll), L(lr)) =>
        ll.zip(lr).flatMap(compare).headOption.orElse(if (ll.size != lr.size) Some(ll.size < lr.size) else None)
      case (i: I, l: L) => compare((L(List(i)), l))
      case (l: L, i: I) => compare((l, L(List(i))))
      case _            => None
    }
  }

  def run(data: String) = {
    val in = stringSeq(data)

    val p1 = in
      .filterNot(_.isEmpty)
      .map(parse)
      .grouped(2)
      .map(x => x.head -> x.last)
      .map(compare)
      .zipWithIndex
      .filter(_._1.contains(true))
      .map(_._2 + 1)
      .sum
    println(p1)

    val p2Packs = List("[[2]]", "[[6]]").map(parse)

    val p2 = (in.filterNot(_.isEmpty).map(parse) ++ p2Packs)
      .sortWith((a, b) => compare((a, b)).getOrElse(false))
      .zipWithIndex
      .filter(p2Packs contains _._1)
      .map(_._2 + 1)
      .product
    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
