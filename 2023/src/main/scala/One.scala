import lib.Support

import scala.util.Try
import scala.annotation.tailrec

object `1` extends App with Support:
  val nums = """(oneight|twone|fiveight|sevenine|eightwo|eighthree|nineight|one|two|three|four|five|six|seven|eight|nine)""".r
  val testData =
    """
      |two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen
      |""".trim.stripMargin
  val input = load

  @tailrec def replaceStringNums(x: String): String =
    nums.findFirstIn(x) match
      case None => x
      case Some("one") => replaceStringNums(x.replaceFirst("one", "1"))
      case Some("two") => replaceStringNums(x.replaceFirst("two", "2"))
      case Some("three") => replaceStringNums(x.replaceFirst("three", "3"))
      case Some("four") => replaceStringNums(x.replaceFirst("four", "4"))
      case Some("five") => replaceStringNums(x.replaceFirst("five", "5"))
      case Some("six") => replaceStringNums(x.replaceFirst("six", "6"))
      case Some("seven") => replaceStringNums(x.replaceFirst("seven", "7"))
      case Some("eight") => replaceStringNums(x.replaceFirst("eight", "8"))
      case Some("nine") => replaceStringNums(x.replaceFirst("nine", "9"))
      case Some("oneight") => replaceStringNums(x.replaceFirst("oneight", "18"))
      case Some("twone") => replaceStringNums(x.replaceFirst("twone", "21"))
      case Some("fiveight") => replaceStringNums(x.replaceFirst("fiveight", "58"))
      case Some("sevenine") => replaceStringNums(x.replaceFirst("sevenine", "79"))
      case Some("eightwo") => replaceStringNums(x.replaceFirst("eightwo", "82"))
      case Some("eighthree") => replaceStringNums(x.replaceFirst("eighthree", "83"))
      case Some("nineight") => replaceStringNums(x.replaceFirst("nineight", "98"))
  end replaceStringNums
    

  def run(data: String) = {
    val in = stringSeq(data)

    lazy val p1  = in.map(line => {
      val nums = line.filter(_.isDigit)
      s"${nums.head}${nums.last}".toLong
    }).sum

    lazy val p2 = in.map(line => {
      val nums = replaceStringNums(line)
        .filter(_.isDigit)
      println(nums)
      s"${nums.head}${nums.last}".toLong
    }).sum

    //println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)

