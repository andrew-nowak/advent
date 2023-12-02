import lib.Support

object D1 extends App with Support:
  // lookahead to enable overlapping returns
  val numberRegex = """(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))""".r
  val testData =
    """
      |two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen
      |eightwo
      |""".trim.stripMargin
  val input = load

  def run(data: String) = {
    val in = stringSeq(data)

    lazy val p1 = in
      .map(line => {
        val nums = line.filter(_.isDigit)
        s"${nums.head}${nums.last}".toLong
      })
      .sum

    lazy val p2 = in
      .map(line => {
        val nums = numberRegex
          .findAllMatchIn(line)
          .map(_.group(1) match {
            case "one"   => 1
            case "two"   => 2
            case "three" => 3
            case "four"  => 4
            case "five"  => 5
            case "six"   => 6
            case "seven" => 7
            case "eight" => 8
            case "nine"  => 9
            case n       => n.toInt
          })
          .toList

        s"${nums.head}${nums.last}".toLong
      })
      .sum

    // println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
