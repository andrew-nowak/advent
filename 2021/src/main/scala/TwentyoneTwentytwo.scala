package twentyone.twentytwo

import lib.{Coord3d, Support}
import java.lang.Math.{max, min}

final case class Step(action: String, cubes: Set[Coord3d])
final case class Cuboid(ax: Int, bx: Int, ay: Int, by: Int, az: Int, bz: Int) {
  def intersection(o: Cuboid): Option[Cuboid] = {
    val ns = Cuboid(max(ax, o.ax), min(bx, o.bx), max(ay, o.ay), min(by, o.by), max(az, o.az), min(bz, o.bz))
    if (ns.ax > ns.bx || ns.ay > ns.by || ns.az > ns.bz) None else Some(ns)
  }

  def volume: Long = (bx - ax + 1).toLong * (by - ay + 1).toLong * (bz - az + 1).toLong
}

object TwentyoneTwentytwo extends App with Support {
  val i = loadStringSeq

  def getCubes(minx: Int, maxx: Int, miny: Int, maxy: Int, minz: Int, maxz: Int): Seq[Coord3d] = {
    for {
      x <- Math.max(minx, -50) to Math.min(maxx, 50)
      y <- Math.max(miny, -50) to Math.min(maxy, 50)
      z <- Math.max(minz, -50) to Math.min(maxz, 50)
    } yield Coord3d(x, y, z)
  }

  val limit = getCubes(-50, 50, -50, 50, -50, 50).toSet

  val steps = i.map { case s"${action} x=${minx}..${maxx},y=${miny}..${maxy},z=${minz}..${maxz}" =>
    Step(action, getCubes(minx.toInt, maxx.toInt, miny.toInt, maxy.toInt, minz.toInt, maxz.toInt).toSet)
  }

  val areActive = steps.foldLeft(Set.empty[Coord3d])((active, step) => {
    if (step.action == "on") active union (step.cubes intersect limit)
    else if (step.action == "off") active diff (step.cubes intersect limit)
    else throw new Exception(s"didn't understand action ${step.action}")
  })

  val part1 = areActive.size
  println(part1)

  final case class OnState(volume: Cuboid, value: Int)

  val stepsp2 = i.foldLeft(Seq.empty[OnState]) { (acc, inst) =>
    inst match {
      case s"${action} x=${minx}..${maxx},y=${miny}..${maxy},z=${minz}..${maxz}" =>
        val cuboid = Cuboid(minx.toInt, maxx.toInt, miny.toInt, maxy.toInt, minz.toInt, maxz.toInt)
        val intersectingStates =
          acc.flatMap(state => state.volume.intersection(cuboid).map(intersection => state -> intersection))
        if (intersectingStates.isEmpty) {
          if (action == "on") acc :+ OnState(cuboid, 1) else /* action == off */ acc
        } else {
          if (action == "on") {
            (acc :+ OnState(cuboid, 1)) ++ intersectingStates.map {
              case (OnState(_, 1), intersection)  => OnState(intersection, -1)
              case (OnState(_, -1), intersection) => OnState(intersection, 1)
            }
          } else {
            acc ++ intersectingStates.map {
              case (OnState(_, 1), intersection)  => OnState(intersection, -1)
              case (OnState(_, -1), intersection) => OnState(intersection, 1)
            }
          }
        }
    }
  }

  val part2 = stepsp2.map(st => st.volume.volume * st.value.toLong).sum

  println(part2)
}
