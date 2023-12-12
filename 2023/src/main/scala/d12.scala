import lib.Support

object d12 extends App with Support {
  val testData =
    """
      |???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1
      |""".trim.stripMargin
  val input = load

  final case class State(springs: String, ns: Seq[Int])

  def countArrangements(
      st: State,
      memo: Map[State, Int]
  ): (Int, Map[State, Int]) = {
    // println(st)
    if (memo.contains(st)) (memo.apply(st), memo)
    else if (st.ns.isEmpty && !st.springs.contains('#')) {
      // println("1")
      (1, memo + (st -> 1))
    } else if (st.ns.isEmpty || st.springs.isEmpty()) {
      // println("empty but 0")
      (0, memo + (st -> 0))
    } else if (st.springs.head == '.') {
      countArrangements(State(st.springs.dropWhile(_ == '.'), st.ns), memo)
    } else if (st.springs.head == '#') {
      val (headSprings, rest) = st.springs.splitAt(st.ns.head)
      if (
        headSprings.size == st.ns.head &&
        headSprings.forall(_ != '.') &&
        rest.headOption.forall(_ != '#')
      ) {
        val (n, newMemo) = countArrangements(
          State(rest.replaceFirst("""^\?""", "."), st.ns.tail),
          memo
        )
        (n, newMemo + (st -> n))
      } else {
        (0, memo + (st -> 0))
      }
    } else { // st.springs.head == '?'
      val tryYes = countArrangements(
        State(st.springs.replaceFirst("""\?""", "#"), st.ns),
        memo
      )
      val tryNo =
        countArrangements(State(st.springs.tail, st.ns), memo ++ tryYes._2)

      // println(st, tryYes._1, tryNo._1)
      (tryYes._1 + tryNo._1, tryNo._2)
    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data)

    val p1 = in
      .map { s =>
        val Array(springs, ns) = s.split(" ")
        State(springs, intSeq(ns, ","))
      }
      .map(countArrangements(_, Map.empty))
      .map(_._1)
      .sum

    val p2 = in.size

    println(p1)

//    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
