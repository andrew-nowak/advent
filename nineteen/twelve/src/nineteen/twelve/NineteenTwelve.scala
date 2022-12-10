package nineteen.twelve

import lib.Support
import lib.Coord3d
import scala.annotation.tailrec

object NineteenTwelve extends App with Support {
  val testData =
    """
      |<x=-1, y=0, z=2>
      |<x=2, y=-10, z=-7>
      |<x=4, y=-8, z=8>
      |<x=3, y=5, z=-1>
      |""".stripMargin.trim
  val input = load
  val inputR = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r

  type Moon = (Coord3d, Coord3d)

  @tailrec def step(n: Int, ms: Seq[Moon]): Seq[Moon] = {
    if (n == 0)
      ms
    else {
      val newMoons = ms.map { case (pos, vel) =>
        val newVel = (ms).foldLeft(vel) { case (acc, (Coord3d(ox, oy, oz), _)) =>
          acc + Coord3d(clamp(ox - pos.x), clamp(oy - pos.y), clamp(oz - pos.z))
        }
        val newPos = pos + newVel
        newPos -> newVel
      }
      step(n - 1, newMoons)
    }
  }

  // find the interval of each axis independently, then find the lcm
  @tailrec def p2(
      ms: Seq[Moon],
      targets: (Seq[Int], Seq[Int], Seq[Int]),
      it: Long,
      seen: (Option[Long], Option[Long], Option[Long])
  ): Long = {
    seen match {
      case (Some(x), Some(y), Some(z)) =>
        lcm(x, lcm(y, z))
      case _ =>
        val newMs = step(1, ms)
        val seenX = seen._1.orElse(if (ms.map(_._1.x) == targets._1) Some(it) else None)
        val seenY = seen._2.orElse(if (ms.map(_._1.y) == targets._2) Some(it) else None)
        val seenZ = seen._3.orElse(if (ms.map(_._1.z) == targets._3) Some(it) else None)
        p2(newMs, targets, it + 1, (seenX, seenY, seenZ))
    }
  }

  def energy(m: Coord3d): Long = (Math.abs(m.x) + Math.abs(m.y) + Math.abs(m.z)).toLong

  def run(data: String) = {
    val in = stringSeq(data)
    val moons = in.map { case inputR(x, y, z) => Coord3d(x.toInt, y.toInt, z.toInt) -> Coord3d(0, 0, 0) }

    val system = (step(1000, moons))
    val p1 = system.map { case (pos, vel) => energy(pos) * energy(vel) }.sum
    println(p1)

    val initialX = moons.map(_._1.x)
    val initialY = moons.map(_._1.y)
    val initialZ = moons.map(_._1.z)

    println(p2(step(1, moons), (initialX, initialY, initialZ), 2, (None, None, None)))
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
