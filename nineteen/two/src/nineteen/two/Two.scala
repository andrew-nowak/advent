package nineteen.two

import lib.Intcode
import lib.Support

object Two extends App with Support {
  val in = loadIntSeq(",")

  val part1 = Intcode.buildAndRunWithPhrase(in, 12, 2)

  println(part1.memory(0))

  val phrases = for {
    noun <- 0 until 100
    verb <- 0 until 100
  } yield noun -> verb

  val part2 = phrases
    .find { case (noun, verb) =>
      Intcode.buildAndRunWithPhrase(in, noun, verb).memory(0) == 19690720L
    }
    .map { case (noun, verb) => 100 * noun + verb }
    .get

  println(part2)
}
