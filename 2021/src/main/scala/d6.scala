import lib.Support

import scala.annotation.tailrec

object d6 extends App with Support {
  @tailrec def simulate(fish: Map[Int, Long], remainingDays: Int): Long = {
    if (remainingDays <= 0)
      fish.values.sum
    else {
      val nextfish = fish.toSeq
        .flatMap {
          case (0, n)   => Seq((6, n), (8, n))
          case (age, n) => Seq((age - 1, n))
        }
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.sum)
        .toMap

      simulate(nextfish, remainingDays - 1)
    }
  }

  val i = loadIntSeq(",")

  val fish = i.groupBy(age => age).view.mapValues(_.size.toLong).toMap

  println(simulate(fish, 80))
  println(simulate(fish, 256))
}
