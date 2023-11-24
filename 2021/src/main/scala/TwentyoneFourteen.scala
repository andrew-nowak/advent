package twentyone.fourteen

import lib.Support

import scala.annotation.tailrec

object TwentyoneFourteen extends App with Support {
  val i = loadStringSeq

  val template = i.head

  val pairInsertions = i
    .drop(2)
    .map { case s"${pair} -> ${insertion}" =>
      pair -> insertion
    }
    .toMap

  def step(rules: Map[String, String])(polymer: String): String = {
    polymer.head.toString + polymer.sliding(2).map(pair => rules(pair) + pair.last).mkString
  }

  val afterTen = LazyList.iterate(template)(step(pairInsertions)).apply(10)

  val dist = afterTen.groupBy(c => c)
  val freqs = dist.values.map(_.length)

  val part1 = freqs.max - freqs.min

  println(part1)

  def runRecstep(template: String, rules: Map[String, String], depth: Int): Seq[Long] = {
    val inputs = template.sliding(2).map(_ -> depth).toList
    val complete = recstep(inputs, Map.empty, rules)
    val done = mergeAnswers(
      inputs.map(complete) :+ template.tail.dropRight(1).groupBy(c => c).view.mapValues(-_.length.toLong).toMap
    )
    done.values.toSeq
  }

  def mergeAnswers(answers: Seq[Map[Char, Long]]): Map[Char, Long] = {
    answers.reduce((l, r) =>
      l.foldLeft(r)((acc, elem) =>
        acc.updatedWith(elem._1) {
          case Some(count) => Some(count + elem._2)
          case _           => Some(elem._2)
        }
      )
    )
  }

  @tailrec
  def recstep(
      stack: List[(String, Int)],
      answers: Map[(String, Int), Map[Char, Long]],
      rules: Map[String, String]
  ): Map[(String, Int), Map[Char, Long]] = {
    stack match {
      case Nil                                    => answers
      case head :: rest if answers.contains(head) => recstep(rest, answers, rules)
      case (pair, 0) :: rest if pair.head != pair.last =>
        recstep(rest, answers.updated((pair, 0), pair.map(_ -> 1L).toMap), rules)
      case (pair, 0) :: rest => recstep(rest, answers.updated((pair, 0), Map(pair.head -> 2)), rules)
      case (head @ (pair, depth)) :: rest =>
        val production = rules.apply(pair)
        val nextDown = List(pair.head.toString + production, production + pair.last).map(_ -> (depth - 1))
        if (nextDown.forall(answers.contains)) {
          val answer = mergeAnswers(nextDown.map(answers))
          val correctedAnswer = answer.updated(production.head, answer(production.head) - 1)
          recstep(rest, answers.updated(head, correctedAnswer), rules)
        } else {
          recstep(nextDown ++ stack, answers, rules)
        }
    }
  }

  val freqsAfterTen = runRecstep(template, pairInsertions, 10)
  val part1v2 = freqsAfterTen.max - freqsAfterTen.min
  println(part1v2)

  val freqsAfterForty = runRecstep(template, pairInsertions, 40)
  val part2 = freqsAfterForty.max - freqsAfterForty.min
  println(part2)
}
