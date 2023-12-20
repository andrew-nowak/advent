import lib.SeqExtras.SeqExtensions
import lib._

import scala.annotation.tailrec

object d19 extends App with Support {
  final case class Rule(att: String, cond: String, num: Int, dest: String)
  final case class Workflow(name: String, rules: Seq[Rule], fallback: String)
  type Ranges = Map[String, (Int, Int)]

  val workflowR = """(\w+)\{(.*),(\w+)}""".r
  val ruleR = """([xmas])([<>])(\d+):(\w+)""".r
  val partR = """([xmas])=(\d+)""".r
  val testData =
    """
      |px{a<2006:qkq,m>2090:A,rfg}
      |pv{a>1716:R,A}
      |lnx{m>1548:A,A}
      |rfg{s<537:gd,x>2440:R,A}
      |qs{s>3448:A,lnx}
      |qkq{x<1416:A,crn}
      |crn{x>2662:A,R}
      |in{s<1351:px,qqz}
      |qqz{s>2770:qs,m<1801:hdj,R}
      |gd{a>3333:R,R}
      |hdj{m>838:A,pv}
      |
      |{x=787,m=2655,a=1222,s=2876}
      |{x=1679,m=44,a=2067,s=496}
      |{x=2036,m=264,a=79,s=2244}
      |{x=2461,m=1339,a=466,s=291}
      |{x=2127,m=1623,a=2188,s=1013}""".stripMargin.trim
  val input = load

  @tailrec def accepts(
      atts: Map[String, Int],
      workflows: Map[String, Workflow],
      wf: String
  ): Boolean = {
    val workflow = workflows(wf)
    val next = workflow.rules
      .collectFirst {
        case Rule(att, "<", num, dest) if atts(att) < num => dest
        case Rule(att, ">", num, dest) if atts(att) > num => dest
      }
      .getOrElse(workflow.fallback)
    next match {
      case "R" => false
      case "A" => true
      case o   => accepts(atts, workflows, o)
    }
  }

  final case class State(r: Ranges, wf: String)
  @tailrec def findRanges(
      q: List[State],
      workflows: Map[String, Workflow],
      tot: Long
  ): Long = {
    q match {
      case Nil => tot
      case State(r, wf) :: t =>
        val (nextStates_, otherwise) = workflows(wf).rules.thread(r) {
          case (Rule(att, ">", num, dest), lastRanges) =>
            val (oldMin, oldMax) = lastRanges.apply(att)
            val newMin = math.max(oldMin, num + 1)
            val newRange = (newMin, oldMax)
            val s = State(lastRanges.updated(att, newRange), dest)
            val o = (oldMin, newMin - 1)
            (s, lastRanges.updated(att, o))
          case (Rule(att, "<", num, dest), lastRanges) =>
            val (oldMin, oldMax) = lastRanges.apply(att)
            val newMax = math.min(oldMax, num - 1)
            val newRange = (oldMin, newMax)
            val s = State(lastRanges.updated(att, newRange), dest)
            val o = (newMax + 1, oldMax)
            (s, lastRanges.updated(att, o))
        }
        val nextStates = nextStates_ :+ State(otherwise, workflows(wf).fallback)
        val (accepted, unaccepted) = nextStates.partition(_.wf == "A")
        val unrejected = unaccepted.filterNot(_.wf == "R")
        val newCombs = accepted
          .map(s =>
            s.r.values
              .map(range => range._2.toLong - range._1.toLong + 1)
              .product
          )
          .sum
        findRanges(t ++ unrejected, workflows, tot + newCombs)
    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data)

    val (workflowsRaw, partsRaw) = in.span(_ != "")

    val workflows: Map[String, Workflow] = workflowsRaw.map {
      case workflowR(name, rulesRaw, fallback) =>
        val rules = rulesRaw.split(",").map {
          case ruleR(att, cond, num, dest) => Rule(att, cond, num.toInt, dest)
        }

        name -> (if (rules.forall(_.dest == fallback))
                   Workflow(name, Seq.empty, fallback)
                 else Workflow(name, rules, fallback))
    }.toMap

    val parts: Seq[Map[String, Int]] = partsRaw.tail.map(
      _.tail
        .dropRight(1)
        .split(",")
        .map { case partR(att, n) => att -> n.toInt }
        .toMap
    )

    val p1 = parts
      .filter(atts => {
        accepts(atts, workflows, "in")
      })
      .map(p => p.values.sum.toLong)
      .sum
    println(p1)

//    println(3999L * 3999L * 3999L * 3999L)
//    println(167409079868000L)
    val initialRanges: Ranges =
      Seq("x", "m", "a", "s").map((_, (1, 4000))).toMap
    val p2 = findRanges(List(State(initialRanges, "in")), workflows, 0L)
    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
