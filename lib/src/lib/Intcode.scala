package lib

import scala.annotation.tailrec

case class Operation(opcode: Int, parameterModes: Seq[Int])

case class Intcode(
    memory: Seq[Int],
    instructionPointer: Int,
    input: List[Int] = Nil,
    output: List[Int] = Nil
) {
  private def getMode(opcode: Int, position: Int) =
    (opcode / pow10(position + 1)) & 1
  private def resolve(opcode: Int, position: Int, writing: Boolean = false) = {
    val mode = getMode(opcode, position)
    require(
      !writing || mode == 0,
      s"In opcode $opcode, arg in position $position must be position mode, but got immediate mode"
    )
    mode match {
      case 0 if writing => memory(instructionPointer + position)
      case 0            => memory(memory(instructionPointer + position))
      case 1            => memory(instructionPointer + position)
    }
  }
  private def pow10(n: Int) = Seq.fill(n)(10).product

  def add(opcode: Int): Intcode = {
    val inputA = resolve(opcode, 1)
    val inputB = resolve(opcode, 2)
    val outputLoc = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(outputLoc, inputA + inputB),
      instructionPointer = instructionPointer + 4
    )
  }

  def mul(opcode: Int): Intcode = {
    val inputA = resolve(opcode, 1)
    val inputB = resolve(opcode, 2)
    val outputLoc = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(outputLoc, inputA * inputB),
      instructionPointer = instructionPointer + 4
    )
  }

  def takeInput(opcode: Int): Intcode = {
    val writeTo = resolve(opcode, 1, writing = true)

    this.copy(
      memory = memory.updated(writeTo, input.head),
      instructionPointer = instructionPointer + 2,
      input = input.tail
    )
  }

  def sendOutput(opcode: Int): Intcode = {
    val value = resolve(opcode, 1)
    this.copy(
      instructionPointer = instructionPointer + 2,
      output = output :+ value
    )
  }

  def jumpIfTrue(opcode: Int): Intcode = {
    val cond = resolve(opcode, 1)
    val dest = resolve(opcode, 2)

    this.copy(instructionPointer =
      if (cond != 0) dest else instructionPointer + 3
    )
  }

  def jumpIfFalse(opcode: Int): Intcode = {
    val cond = resolve(opcode, 1)
    val dest = resolve(opcode, 2)

    this.copy(instructionPointer =
      if (cond == 0) dest else instructionPointer + 3
    )
  }

  def lessThan(opcode: Int): Intcode = {
    val x = resolve(opcode, 1)
    val y = resolve(opcode, 2)
    val dest = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(dest, if (x < y) 1 else 0),
      instructionPointer = instructionPointer + 4
    )
  }

  def equal(opcode: Int): Intcode = {
    val x = resolve(opcode, 1)
    val y = resolve(opcode, 2)
    val dest = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(dest, if (x == y) 1 else 0),
      instructionPointer = instructionPointer + 4
    )
  }
}

object Intcode {
  @tailrec
  def run(intcode: Intcode): Intcode = {
    val opcode = intcode.memory(intcode.instructionPointer)
    opcode % 100 match {
      case 99 => intcode
      case 1  => run(intcode.add(opcode))
      case 2  => run(intcode.mul(opcode))
      case 3  => run(intcode.takeInput(opcode))
      case 4  => run(intcode.sendOutput(opcode))
      case 5  => run(intcode.jumpIfTrue(opcode))
      case 6  => run(intcode.jumpIfFalse(opcode))
      case 7  => run(intcode.lessThan(opcode))
      case 8  => run(intcode.equal(opcode))
    }
  }

  def buildAndRun(memory: Seq[Int]): Intcode = {
    val ic = new Intcode(memory, 0)
    run(ic)
  }

  def buildAndRunWithPhrase(memory: Seq[Int], noun: Int, verb: Int): Intcode =
    buildAndRun(memory.updated(1, noun).updated(2, verb))

  def buildAndRunWithIo(memory: Seq[Int], input: List[Int]): List[Int] =
    run(Intcode(memory, 0, input)).output
}
