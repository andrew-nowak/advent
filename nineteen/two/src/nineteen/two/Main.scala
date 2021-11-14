package nineteen.two

import lib.Intcode
import lib.Support

object Main extends App with Support {
  val in = loadIntSeq(",")

  val part1 = Intcode.buildAndRunWithInputs(in, 12, 2)

  println(part1.memory.head)

  val phrases = for {
    noun <- 0 until 100
    verb <- 0 until 100
  } yield noun -> verb

  val part2 = phrases
    .find { case (noun, verb) =>
      Intcode.buildAndRunWithInputs(in, noun, verb).memory.head == 19690720
    }
    .map { case (noun, verb) => 100 * noun + verb }
    .get

  println(part2)
}
