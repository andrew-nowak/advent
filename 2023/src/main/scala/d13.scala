import lib.Support

object d13 extends App with Support {
  val testData =
    """
      |#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#
      |""".trim.stripMargin
  val input = load

  def findReflection(block: Seq[Seq[Char]], not: Option[Int]): Option[Int] =
    (1 until block.length).filterNot(not.contains).find { offset =>
      val (a, b) = block.splitAt(offset)
      a.reverse.zip(b).forall(x => x._1 == x._2)
    }

  def swap(x: Char): Char =
    if (x == '#') '.'
    else if (x == '.') '#'
    else throw new RuntimeException("aaaa")

  def solve(block: Seq[Seq[Char]], not: Option[Int] = None): Option[Int] =
    findReflection(block, not.map(_ / 100))
      .map(_ * 100)
      .orElse(findReflection(block.transpose, not))

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeqs(data).map(block => block.map(_.toCharArray.toSeq))

    val lines = in.map { block => solve(block).get }

    val p1 = lines.sum

    val p2 = in
      .zip(lines)
      .map { case (block, line) =>
        (for {
          y <- block.indices
          x <- block.head.indices
          soln <- solve(
            block.updated(y, block(y).updated(x, swap(block(y)(x)))),
            not = Some(line)
          )
        } yield soln).head
      }
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
