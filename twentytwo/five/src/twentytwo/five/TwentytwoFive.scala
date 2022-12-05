package twentytwo.five

import lib.Support

import scala.util.Try

object TwentytwoFive extends App with Support {
  val testData =
    """
      |    [D]
      |[N] [C]
      |[Z] [M] [P]
      | 1   2   3
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2
      |""".trim.stripMargin
  val input = load

  def run(data: String) = {
    val in = stringSeq(data)

    val nStacks = in.find(!_.contains('[')).get.split("\\D+").flatMap(_.toIntOption).max

    val stacks = Seq.fill(nStacks)(Seq.empty[Char])

    val contents = in.takeWhile(_.contains('[')).foldLeft(stacks)((stacks, line) => {
      stacks.zipWithIndex.map {
        case (st, i) =>
          Try(line.charAt(4 * i + 1)).toOption.filterNot(_ == ' ').map(st :+ _).getOrElse(st)
      }
    })

    val instructionR = """move (\d+) from (\d+) to (\d+)""".r
    val instructions = in.dropWhile(!_.startsWith("move")).flatMap {
      case instructionR(n, from, to) => Seq.fill(n.toInt)((from.toInt - 1, to.toInt - 1))
    }
    val p1 = instructions.foldLeft(contents)((stacks, instr) => {
      val (from, to) = instr
      val x = stacks(from)
      val y = stacks(to)

      stacks.updated(from, x.tail).updated(to, x.head +: y)
    }).map(_.head).mkString

    val p2Instructions = in.dropWhile(!_.startsWith("move")).map {
      case instructionR(n, from, to) => (n.toInt, from.toInt - 1, to.toInt - 1)
    }

    val p2 = p2Instructions.foldLeft(contents)((stacks, instr)=> {
      val (n, from, to) = instr
      val x = stacks(from)
      val y = stacks(to)
      stacks.updated(from, x.drop(n)).updated(to, x.take(n) ++ y)
    }).map(_.head).mkString

    println(p1)

    println(p2)
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
