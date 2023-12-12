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
      memo: Map[State, Long]
  ): (Long, Map[State, Long]) = {
    // println(st)
    if (memo.contains(st)) (memo.apply(st), memo)
    else if (st.ns.isEmpty && !st.springs.contains('#')) {
      // println("1")
      (1L, memo + (st -> 1L))
    } else if (st.ns.isEmpty || st.springs.isEmpty()) {
      // println("empty but 0")
      (0L, memo + (st -> 0L))
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
        (0L, memo + (st -> 0L))
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

    val initialSpringMap = in
      .map { s =>
        val Array(springs, ns) = s.split(" ")
        State(springs, intSeq(ns, ","))
      }

    val p1 = initialSpringMap
      .map(countArrangements(_, Map.empty))
      .map(_._1)
      .sum

    val fullSpringMap = initialSpringMap
      .map { case State(springs, ns) =>
        State(
          Seq.fill(5)(springs).mkString("?"),
          Seq.fill(5)(ns).flatten
        )
      }

    val p2 = fullSpringMap
      .map(countArrangements(_, Map.empty))
      .map(_._1)
      .sum

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
