package twentyone.eight

import lib.Support

object TwentyoneEight extends App with Support {
  val numbers =
    """
      |abcefg
      |cf
      |acdeg
      |acdfg
      |bcdf
      |abdfg
      |abdefg
      |acf
      |abcdefg
      |abcdfg
      |""".stripMargin.trim.split("\\n")

  def createTranslation(pattern: String): Map[String, String] = {
    val dist: Map[String, Int] = pattern
      .split("")
      .filterNot(_ == " ")
      .foldLeft(Map.empty[String, Int])((map, letter) => map.updatedWith(letter)(_.map(_ + 1).orElse(Some(1))))

    val encodedNumbers = pattern.split(" ")

    val one = encodedNumbers.find(_.length == 2).get.split("")
    val seven = encodedNumbers.find(_.length == 3).get.split("")
    val four = encodedNumbers.find(_.length == 4).get.split("")

    val f = dist.find(_._2 == 9).get._1
    val e = dist.find(_._2 == 4).get._1
    val b = dist.find(_._2 == 6).get._1
    val c = one.filterNot(_ == f).head
    val a = seven.filterNot(Seq(c, f).contains).head
    val d = four.filterNot(Seq(b, c, f).contains).head
    val g = Seq("a", "b", "c", "d", "e", "f", "g").filterNot(Seq(a, b, c, d, e, f).contains).head

    Map(a -> "a", b -> "b", c -> "c", d -> "d", e -> "e", f -> "f", g -> "g")
  }
  val i = loadStringSeq.map(_.split(" \\| ").toList)

  val easyDigits = Set(2, 3, 4, 7)
  val part1 = i.map(_.last.split(" ").map(_.length).count(easyDigits.contains)).sum

  val part2 = i
    .map(note => {
      val signal = note.head
      val translation = createTranslation(signal).toMap

      val output = note.last
      val translated = output.split("").map(letter => translation.getOrElse(letter, letter)).mkString

      translated.split(" ").map(w => numbers.indexOf(w.sorted)).mkString.toInt
    })
    .sum

  println(part1)
  println(part2)
}
