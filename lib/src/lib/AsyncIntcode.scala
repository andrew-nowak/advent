package lib

import scala.concurrent.{ExecutionContext, Future, Promise}

// TODO type param on receiver
case class AsyncIntcode(
    memory: Seq[Int],
    instructionPointer: Int,
    input: Provider = Provider.nop,
    output: Receiver = Receiver.nop
)(implicit val executionContext: ExecutionContext) {
  private def getMode(opcode: Int, position: Int): Int =
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

  def add(opcode: Int): AsyncIntcode = {
    val inputA = resolve(opcode, 1)
    val inputB = resolve(opcode, 2)
    val outputLoc = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(outputLoc, inputA + inputB),
      instructionPointer = instructionPointer + 4
    )
  }

  def mul(opcode: Int): AsyncIntcode = {
    val inputA = resolve(opcode, 1)
    val inputB = resolve(opcode, 2)
    val outputLoc = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(outputLoc, inputA * inputB),
      instructionPointer = instructionPointer + 4
    )
  }

  def takeInput(opcode: Int): Future[AsyncIntcode] = {
    val writeTo = resolve(opcode, 1, writing = true)

    input.take.map(value => {
      this.copy(
        memory = memory.updated(writeTo, value),
        instructionPointer = instructionPointer + 2
      )
    })
  }

  def sendOutput(opcode: Int): AsyncIntcode = {
    val value = resolve(opcode, 1)
    output.give(value)
    this.copy(
      instructionPointer = instructionPointer + 2
    )
  }

  def jumpIfTrue(opcode: Int): AsyncIntcode = {
    val cond = resolve(opcode, 1)
    val dest = resolve(opcode, 2)

    this.copy(instructionPointer = if (cond != 0) dest else instructionPointer + 3)
  }

  def jumpIfFalse(opcode: Int): AsyncIntcode = {
    val cond = resolve(opcode, 1)
    val dest = resolve(opcode, 2)

    this.copy(instructionPointer = if (cond == 0) dest else instructionPointer + 3)
  }

  def lessThan(opcode: Int): AsyncIntcode = {
    val x = resolve(opcode, 1)
    val y = resolve(opcode, 2)
    val dest = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(dest, if (x < y) 1 else 0),
      instructionPointer = instructionPointer + 4
    )
  }

  def equal(opcode: Int): AsyncIntcode = {
    val x = resolve(opcode, 1)
    val y = resolve(opcode, 2)
    val dest = resolve(opcode, 3, writing = true)

    this.copy(
      memory = memory.updated(dest, if (x == y) 1 else 0),
      instructionPointer = instructionPointer + 4
    )
  }
}

object AsyncIntcode {
  def run(intcode: AsyncIntcode)(implicit executionContext: ExecutionContext): Future[AsyncIntcode] = {
    val opcode = intcode.memory(intcode.instructionPointer)
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
    }
  }

  def buildAndRun(memory: Seq[Int])(implicit executionContext: ExecutionContext): Future[AsyncIntcode] = {
    val ic = new AsyncIntcode(memory, 0)
    run(ic)
  }

  def buildAndRunWithPhrase(memory: Seq[Int], noun: Int, verb: Int)(implicit
      executionContext: ExecutionContext
  ): Future[AsyncIntcode] =
    buildAndRun(memory.updated(1, noun).updated(2, verb))

  def buildAndRunWithIo(memory: Seq[Int], input: List[Int])(implicit
      executionContext: ExecutionContext
  ): Future[List[Int]] =
    run(AsyncIntcode(memory, 0, new ListProvider(input), new ListReceiver))
      .map(_.output.asInstanceOf[ListReceiver].output)

  def buildAndRunWithBuffers(memory: Seq[Int], input: Provider, output: Receiver)(implicit
      executionContext: ExecutionContext
  ): Future[Receiver] = {
    run(AsyncIntcode(memory, 0, input, output)).map(_.output)
  }
}

trait Provider {
  def take: Future[Int]
}
object Provider {
  def nop: Provider = new Provider {
    override def take: Future[Int] = Future.failed(new IllegalArgumentException("can't get input from a nop provider"))
  }
}

class ListProvider(private var ns: List[Int]) extends Provider {
  override def take: Future[Int] = {
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
  def give(out: Int): Unit
}
object Receiver {
  def nop: Receiver = _ => ()
}

class ListReceiver extends Receiver {
  var output: List[Int] = Nil

  override def give(out: Int): Unit = {
    output = output :+ out
  }
}

class IntcodeBuffer(implicit ec: ExecutionContext) extends Provider with Receiver {
  val buf = scala.collection.mutable.ListBuffer[Int]()
  val waiting = scala.collection.mutable.ListBuffer[Promise[Int]]()

  override def take: Future[Int] = {
    this.synchronized {
      if (buf.nonEmpty) {
        val n = buf.remove(0)
        Future.successful(n)
      } else {
        val p = Promise[Int]
        waiting.append(p)
        p.future
      }
    }
  }

  override def give(out: Int): Unit = {
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
