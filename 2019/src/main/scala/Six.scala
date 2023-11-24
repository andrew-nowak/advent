package nineteen.six

import lib.Support

import scala.annotation.tailrec

object Six extends App with Support {
  @tailrec
  def countOrbits(
      q: List[(String, Int)] = List(("COM", 0)),
      total: Int = 0
  ): Int = {
    if (q.isEmpty) total
    else {
      val (obj, depth) :: rest = q
      val nq = rest ++ orbitRelationships
        .get(obj)
        .map(_.map((_, depth + 1)))
        .getOrElse(Nil)

      countOrbits(nq, total + depth)
    }
  }

  @tailrec
  def countTransfers(
      q: List[(String, Int)],
      seen: Set[String],
      target: String
  ): Int = {
    val (obj, depth) :: rest = q
    if (obj == target) depth
    else if (seen contains obj) countTransfers(rest, seen, target)
    else {
      val next = (orbitRelationships.getOrElse(obj, Nil) ++ orbitsParsed.find(_(1) == obj).map(_.head))
        .filterNot(seen.contains)
        .map((_, depth + 1))
      countTransfers(rest ++ next, seen + obj, target)
    }
  }
  val orbitSpec = loadStringSeq

  val orbitsParsed = orbitSpec.map(_.split("\\)"))
  val orbitRelationships = orbitsParsed.groupMap(_.head)(_.last)

  println(countOrbits())

  val imOrbiting = orbitsParsed.find(_(1) == "YOU").get.head
  val santasOrbiting = orbitsParsed.find(_(1) == "SAN").get.head

  println(countTransfers(List((imOrbiting, 0)), Set.empty, santasOrbiting))

}
