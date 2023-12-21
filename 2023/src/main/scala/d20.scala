import lib._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object d20 extends App with Support {
  private sealed trait Module {
    val dests: Seq[String]
    def apply(high: Boolean, from: String): (Module, Seq[(String, Boolean)])
  }
  private final case class Broadcaster(dests: Seq[String]) extends Module {
    override def apply(
        high: Boolean,
        from: String
    ): (Broadcaster, Seq[(String, Boolean)]) = (this, dests.map((_, high)))
  }
  private final case class Flipflop(on: Boolean, dests: Seq[String])
      extends Module {
    override def apply(
        high: Boolean,
        from: String
    ): (Flipflop, Seq[(String, Boolean)]) = {
      if (high) (this, Seq.empty)
      else {
        val n = this.copy(on = !on)
        (n, dests.map((_, !on)))
      }
    }
  }
  private final case class Conjunction(
      memory: Map[String, Boolean],
      dests: Seq[String]
  ) extends Module {
    override def apply(
        high: Boolean,
        from: String
    ): (Conjunction, Seq[(String, Boolean)]) = {
      val n = this.copy(memory = memory.updated(from, high))
      val emitHigh = !n.memory.values.forall(_ == true)
//      println(n, emitHigh)
      (n, dests.map((_, emitHigh)))
    }
  }
  private final case class Generic() extends Module {
    override val dests: Seq[String] = Seq.empty

    override def apply(
        high: Boolean,
        from: String
    ): (Generic, Seq[(String, Boolean)]) = (this, Seq.empty)
  }

  val testData =
    """broadcaster -> a
      |%a -> inv, con
      |&inv -> b
      |%b -> con
      |&con -> output""".stripMargin.trim
  val input = load

  @tailrec def sim(
      q: Queue[(String, Boolean, String)],
      m: Map[String, Module],
      buttons: Int,
      highs: Long,
      lows: Long
  ): Long = {
    q.dequeueOption match {
      case None if buttons <= 0 => println(highs, lows); highs * lows
      case None =>
        val q =
          Queue.from(m("broadcaster").dests.map((_, false, "broadcaster")))
        sim(q, m, buttons - 1, highs, lows + q.size + 1)
      case Some(((mod, high, from), t)) =>
        m.get(mod) match {
          case Some(module) =>
            val (updatedMod, nextActivations) = module.apply(high, from)
            val nq = t.enqueueAll(nextActivations.map { case (nextMod, high) =>
              (nextMod, high, mod)
            })
            val nm = m.updated(mod, updatedMod)
            val cHighs = nextActivations.count(_._2)
            val cLows = nextActivations.size - cHighs
//            println(mod, if (high) "high" else "low", cHighs, cLows, nextActivations, highs, lows)
            sim(nq, nm, buttons, highs + cHighs, lows + cLows)
          case None => sim(t, m, buttons, highs, lows)
        }

    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data)

    val inputs = in
      .flatMap { mod =>
        val Array(modId, dests_) = mod.split(" -> ")
        val dests = dests_.split(", ")
        if (modId.startsWith("%") || modId.startsWith("&"))
          dests.map((modId.tail, _))
        else dests.map((modId, _))
      }
      .groupMap(_._2)(_._1)
    val graph: Map[String, Module] = in.map { mod =>
      val Array(modId, dests_) = mod.split(" -> ")
      val dests = dests_.split(", ")
      if (modId startsWith "%")
        modId.tail -> Flipflop(on = false, dests = dests)
      else if (modId startsWith "&")
        modId.tail -> Conjunction(
          memory = inputs(modId.tail).map((_, false)).toMap,
          dests = dests
        )
      else if (modId == "broadcaster") modId -> Broadcaster(dests)
      else modId -> Generic()
    }.toMap

    val p1 = sim(Queue.empty, graph, 1000, 0L, 0L)
    println(p1)

    val p2 = in.size
    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
