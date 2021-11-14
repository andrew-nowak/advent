package lib

import scala.annotation.tailrec

case class Intcode(memory: Seq[Int], instructionPointer: Int) {
  def add(): Intcode = {
    val inputA = memory(memory(instructionPointer + 1))
    val inputB = memory(memory(instructionPointer + 2))
    val outputLoc = memory(instructionPointer + 3)

    Intcode(memory.updated(outputLoc, inputA + inputB), instructionPointer + 4)
  }

  def mul(): Intcode = {
    val inputA = memory(memory(instructionPointer + 1))
    val inputB = memory(memory(instructionPointer + 2))
    val outputLoc = memory(instructionPointer + 3)

    Intcode(memory.updated(outputLoc, inputA * inputB), instructionPointer + 4)
  }
}

object Intcode {
  @tailrec
  def run(intcode: Intcode): Intcode = {
    intcode.memory(intcode.instructionPointer) match {
      case 99 => intcode
      case 1  => run(intcode.add())
      case 2  => run(intcode.mul())
    }
  }

  def buildAndRun(memory: Seq[Int]): Intcode = {
    val ic = new Intcode(memory, 0)
    run(ic)
  }

  def buildAndRunWithInputs(memory: Seq[Int], noun: Int, verb: Int): Intcode =
    buildAndRun(memory.updated(1, noun).updated(2, verb))
}
