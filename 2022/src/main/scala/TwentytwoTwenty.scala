package twentytwo.twenty

import lib.Support

object TwentytwoTwenty extends App with Support {
  val testData =
    """
      |1
      |2
      |-3
      |3
      |-2
      |0
      |4
      |""".stripMargin.trim
  val input = load

  def run(data: String) = {
    val in = longSeq(data).zipWithIndex
    val bb = in.map(_._1).min.abs + 1

    val p1 = in.foldLeft(in)((acc, n) => {
      val i = acc.indexOf(n)
      val m = if (n._1 >= 0) n._1 else bb * (in.size - 1) + n._1
      val j = (i + m) % (in.size - 1)
      val (front, back) = acc.patch(i, Nil, 1).splitAt(j.toInt)

      if (j == 0) back ++ List(n) else front ++ List(n) ++ back
    })

    val i0 = p1.indexWhere(_._1 == 0)
    val i1 = p1((i0 + 1000) % p1.size)
    val i2 = p1((i0 + 2000) % p1.size)
    val i3 = p1((i0 + 3000) % p1.size)

    println(i1._1 + i2._1 + i3._1)

    val bbb = in.map(_._1).min.abs * 811589153 + 1

    val p2 = (0 until 10).foldLeft(in.map(y => y._1 * 811589153 -> y._2))((nums, _) => {

      nums
        .sortBy(_._2)
        .foldLeft(nums)((acc, n) => {
          val i = acc.indexOf(n)
          val m = if (n._1 >= 0) n._1 else bbb * (in.size - 1) + n._1
          val j = (i + m) % (in.size - 1)
          val (front, back) = acc.patch(i, Nil, 1).splitAt(j.toInt)

          if (j == 0) back ++ List(n) else front ++ List(n) ++ back
        })

    })

    println(p2)
    val j0 = p2.indexWhere(_._1 == 0)
    val j1 = p2((j0 + 1000) % p2.size)
    val j2 = p2((j0 + 2000) % p2.size)
    val j3 = p2((j0 + 3000) % p2.size)

    println(j1._1 + j2._1 + j3._1)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
