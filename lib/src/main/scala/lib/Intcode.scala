package lib

// TODO type param on receiver
case class Intcode(
    memory: Map[Long, Long],
    instructionPointer: Long,
    input: List[Long] = Nil,
    output: List[Long] = Nil,
    relativeBase: Long = 0
) {
  def memoryAccess(n: Long): Long = memory.getOrElse(n, 0)

  private def getMode(opcode: Long, position: Long): Long =
    (opcode / pow10(position + 1)) % 10
  private def resolve(opcode: Long, position: Int, writing: Boolean = false) = {
    val mode = getMode(opcode, position)
    require(
      !writing || mode != 1,
      s"In opcode $opcode, arg in position $position must be position mode, but got immediate mode"
    )
    mode match {
      case 0 if writing => memoryAccess(instructionPointer + position)
      case 0            => memoryAccess(memoryAccess(instructionPointer + position))
      case 1            => memoryAccess(instructionPointer + position)
      case 2 if writing => relativeBase + memoryAccess(instructionPointer + position)
      case 2            => memoryAccess(relativeBase + memoryAccess(instructionPointer + position))
    }
  }
  private def pow10(n: Long) = Math.pow(10, n.toDouble).toLong

  def add(opcode: Long): Intcode = {
    val inputA = resolve(opcode, 1)
    val inputB = resolve(opcode, 2)
    val outputLoc = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(outputLoc, inputA + inputB),
      instructionPointer = instructionPointer + 4
    )
  }

  def mul(opcode: Long): Intcode = {
    val inputA = resolve(opcode, 1)
    val inputB = resolve(opcode, 2)
    val outputLoc = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(outputLoc, inputA * inputB),
      instructionPointer = instructionPointer + 4
    )
  }

  def jumpIfTrue(opcode: Long): Intcode = {
    val cond = resolve(opcode, 1)
    val dest = resolve(opcode, 2)

    this.copy(instructionPointer = if (cond != 0) dest else instructionPointer + 3)
  }

  def jumpIfFalse(opcode: Long): Intcode = {
    val cond = resolve(opcode, 1)
    val dest = resolve(opcode, 2)

    this.copy(instructionPointer = if (cond == 0) dest else instructionPointer + 3)
  }

  def lessThan(opcode: Long): Intcode = {
    val x = resolve(opcode, 1)
    val y = resolve(opcode, 2)
    val dest = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(dest, if (x < y) 1 else 0),
      instructionPointer = instructionPointer + 4
    )
  }

  def equal(opcode: Long): Intcode = {
    val x = resolve(opcode, 1)
    val y = resolve(opcode, 2)
    val dest = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(dest, if (x == y) 1 else 0),
      instructionPointer = instructionPointer + 4
    )
  }

  def adjustRelativeBase(opcode: Long): Intcode = {
    val t = resolve(opcode, 1)

    this.copy(
      instructionPointer = instructionPointer + 2,
      relativeBase = relativeBase + t
    )
  }

  def takeInput(opcode: Long): Option[Intcode] = {
    val writeTo = resolve(opcode, 1, writing = true)

    input.headOption.map(value => {
      this
        .copy(memory = memory.updated(writeTo, value), instructionPointer = instructionPointer + 2, input = input.tail)
    })
  }

  def sendOutput(opcode: Long): Intcode = {
    val value = resolve(opcode, 1)
    this.copy(
      instructionPointer = instructionPointer + 2,
      output = output :+ value
    )
  }
}

sealed trait IntcodeExit

case class Halt(intcode: Intcode) extends IntcodeExit
case class WaitingForInput(intcode: Intcode) extends IntcodeExit

object Intcode {
  def run(intcode: Intcode): IntcodeExit = {
    val opcode = intcode.memoryAccess(intcode.instructionPointer)
    opcode % 100 match {
      case 99 => Halt(intcode)
      case 1  => run(intcode.add(opcode))
      case 2  => run(intcode.mul(opcode))
      case 3 =>
        intcode.takeInput(opcode) match {
          case Some(outcome) => run(outcome)
          case None          => WaitingForInput(intcode)
        }
      case 4 => run(intcode.sendOutput(opcode))
      case 5 => run(intcode.jumpIfTrue(opcode))
      case 6 => run(intcode.jumpIfFalse(opcode))
      case 7 => run(intcode.lessThan(opcode))
      case 8 => run(intcode.equal(opcode))
      case 9 => run(intcode.adjustRelativeBase(opcode))
    }
  }

  def memoryFromSeq(memory: Seq[Long]): Map[Long, Long] =
    memory.zipWithIndex.map { case (n, i) => (i.toLong, n) }.toMap

  def buildAndRun(memory: Seq[Int]): Intcode = {
    val ic = new Intcode(memoryFromSeq(memory.map(_.toLong)), 0)

    run(ic) match {
      case Halt(outcome) => outcome
      case _             => throw new Exception("intcode halted waiting for input")
    }
  }

  def buildAndRunWithPhrase(memory: Seq[Int], noun: Int, verb: Int): Intcode =
    buildAndRun(memory.updated(1, noun).updated(2, verb))

  def buildAndRunWithIo(memory: Seq[Long], input: List[Long]): List[Long] =
    run(Intcode(memoryFromSeq(memory), 0, input)) match {
      case Halt(outcome) => outcome.output
      case _             => throw new Exception("intcode halted waiting for input")
    }
}
