package lib

import scala.concurrent.{ExecutionContext, Future, Promise}

// TODO type param on receiver
case class AsyncIntcode(
    memory: Map[Long, Long],
    instructionPointer: Long,
    input: Provider = Provider.nop,
    output: Receiver = Receiver.nop,
    relativeBase: Long = 0,
)(implicit val executionContext: ExecutionContext) {
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
      case 2 => memoryAccess(relativeBase + memoryAccess(instructionPointer + position))
    }
  }
  private def pow10(n: Long) = Math.pow(10, n).toLong

  def add(opcode: Long): AsyncIntcode = {
    val inputA = resolve(opcode, 1)
    val inputB = resolve(opcode, 2)
    val outputLoc = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(outputLoc, inputA + inputB),
      instructionPointer = instructionPointer + 4
    )
  }

  def mul(opcode: Long): AsyncIntcode = {
    val inputA = resolve(opcode, 1)
    val inputB = resolve(opcode, 2)
    val outputLoc = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(outputLoc, inputA * inputB),
      instructionPointer = instructionPointer + 4
    )
  }

  def takeInput(opcode: Long): Future[AsyncIntcode] = {
    val writeTo = resolve(opcode, 1, writing = true)

    input.take.map(value => {
      this.copy(
        memory = memory.updated(writeTo, value),
        instructionPointer = instructionPointer + 2
      )
    })
  }

  def sendOutput(opcode: Long): AsyncIntcode = {
    val value = resolve(opcode, 1)
    output.give(value)
    this.copy(
      instructionPointer = instructionPointer + 2
    )
  }

  def jumpIfTrue(opcode: Long): AsyncIntcode = {
    val cond = resolve(opcode, 1)
    val dest = resolve(opcode, 2)

    this.copy(instructionPointer = if (cond != 0) dest else instructionPointer + 3)
  }

  def jumpIfFalse(opcode: Long): AsyncIntcode = {
    val cond = resolve(opcode, 1)
    val dest = resolve(opcode, 2)

    this.copy(instructionPointer = if (cond == 0) dest else instructionPointer + 3)
  }

  def lessThan(opcode: Long): AsyncIntcode = {
    val x = resolve(opcode, 1)
    val y = resolve(opcode, 2)
    val dest = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(dest, if (x < y) 1 else 0),
      instructionPointer = instructionPointer + 4
    )
  }

  def equal(opcode: Long): AsyncIntcode = {
    val x = resolve(opcode, 1)
    val y = resolve(opcode, 2)
    val dest = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(dest, if (x == y) 1 else 0),
      instructionPointer = instructionPointer + 4
    )
  }

  def adjustRelativeBase(opcode: Long): AsyncIntcode = {
    val t = resolve(opcode, 1)

    this.copy(
      instructionPointer = instructionPointer + 2,
      relativeBase = relativeBase + t
    )
  }
}

object AsyncIntcode {
  def run(intcode: AsyncIntcode)(implicit executionContext: ExecutionContext): Future[AsyncIntcode] = {
    val opcode = intcode.memoryAccess(intcode.instructionPointer)
    opcode % 100 match {
      case 99 => Future.successful(intcode)
      case 1  => run(intcode.add(opcode))
      case 2  => run(intcode.mul(opcode))
      case 3  => intcode.takeInput(opcode).flatMap(run)
      case 4  => run(intcode.sendOutput(opcode))
      case 5  => run(intcode.jumpIfTrue(opcode))
      case 6  => run(intcode.jumpIfFalse(opcode))
      case 7  => run(intcode.lessThan(opcode))
      case 8  => run(intcode.equal(opcode))
      case 9 => run(intcode.adjustRelativeBase(opcode))
    }
  }

  def memoryFromSeq(memory: Seq[Long]): Map[Long, Long] =
    memory.zipWithIndex.map { case (n, i) => (i.toLong, n) }.toMap

  def buildAndRun(memory: Seq[Int])(implicit executionContext: ExecutionContext): Future[AsyncIntcode] = {
    val ic = new AsyncIntcode(memoryFromSeq(memory.map(_.toLong)), 0)
    run(ic)
  }

  def buildAndRunWithPhrase(memory: Seq[Int], noun: Int, verb: Int)(implicit
      executionContext: ExecutionContext
  ): Future[AsyncIntcode] =
    buildAndRun(memory.updated(1, noun).updated(2, verb))

  def buildAndRunWithIo(memory: Seq[Long], input: List[Long])(implicit
      executionContext: ExecutionContext
  ): Future[List[Long]] =
    run(AsyncIntcode(memoryFromSeq(memory), 0, new ListProvider(input), new ListReceiver))
      .map(_.output.asInstanceOf[ListReceiver].output)

  def buildAndRunWithBuffers(memory: Seq[Long], input: Provider, output: Receiver)(implicit
      executionContext: ExecutionContext
  ): Future[Receiver] = {
    run(AsyncIntcode(memoryFromSeq(memory), 0, input, output)).map(_.output)
  }
}

trait Provider {
  def take: Future[Long]
}
object Provider {
  def nop: Provider = new Provider {
    override def take: Future[Long] = Future.failed(new IllegalArgumentException("can't get input from a nop provider"))
  }
}

class ListProvider(private var ns: List[Long]) extends Provider {
  override def take: Future[Long] = {
    this.synchronized {
      if (ns.nonEmpty) {
        val head :: nextNs = ns
        ns = nextNs
        Future.successful(head)
      } else {
        Future.failed(new IllegalArgumentException("input consumed"))
      }
    }
  }
}

trait Receiver {
  def give(out: Long): Unit
}
object Receiver {
  def nop: Receiver = _ => ()
}

class ListReceiver extends Receiver {
  var output: List[Long] = Nil

  override def give(out: Long): Unit = {
    output = output :+ out
  }
}

class IntcodeBuffer extends Provider with Receiver {
  private val buf = scala.collection.mutable.ListBuffer[Long]()
  private val waiting = scala.collection.mutable.ListBuffer[Promise[Long]]()

  override def take: Future[Long] = {
    this.synchronized {
      if (buf.nonEmpty) {
        val n = buf.remove(0)
        Future.successful(n)
      } else {
        val p = Promise[Long]
        waiting.append(p)
        p.future
      }
    }
  }

  override def give(out: Long): Unit = {
    this.synchronized {
      if (waiting.nonEmpty) {
        val tip = waiting.remove(0)
        tip.success(out)
      } else {
        buf.append(out)
      }
    }
  }
}
