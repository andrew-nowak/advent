import lib._

import scala.annotation.tailrec

object d22 extends App with Support {
  final case class Brick(private val l: Coord3d, private val r: Coord3d) {
    val Seq(min, max) = Seq(l, r).sortBy(_.manhattan)
//    println(min, max)
    lazy val cubes = (for {
      x <- min.x to max.x
      y <- min.y to max.y
      z <- min.z to max.z
    } yield Coord3d(x, y, z)).toSet

    def drop(n: Int): Brick = {
      val lower = Coord3d(0, 0, -n)
      Brick(l + lower, r + lower)
    }

    override def toString: String = "Brick(" + min + "~" + max + ")"
  }

  val testData =
    """1,0,1~1,2,1
      |0,0,2~2,0,2
      |0,2,3~2,2,3
      |0,0,4~0,2,4
      |2,0,5~2,2,5
      |0,1,6~2,1,6
      |1,1,8~1,1,9""".stripMargin.trim
  val input = load

  final case class State(bs: Seq[Brick], cubes: Set[Coord3d])
  val ns = LazyList.from(1)

  @tailrec def dislodge(cubes: Set[Coord3d], bricks: List[Brick], cache: Map[Brick, Seq[Brick]], falling: Seq[Brick]): Seq[Brick] = {
    bricks match {
      case Nil => falling
      case h :: t if cache contains h =>
        val hit = cache(h)
        val nc = cubes.diff(h.cubes ++ hit.flatMap(_.cubes))
        dislodge(nc, t.diff(hit), cache, falling ++ hit :+ h)
      case h :: t if h.drop(1).cubes.intersect(cubes).isEmpty => // this brick falls
        dislodge(cubes.diff(h.cubes), t, cache, falling :+ h)
      case _ :: t => // this brick stays
        dislodge(cubes, t, cache, falling)
    }
  }

//  def dislodge(bricks: Seq[Brick], brick: Brick): Int = {
//    val without = bricks.filterNot(_ == brick)
//    val remainingCubes = without.flatMap(_.cubes).toSet
//    without.filter(_.min.z > brick.max.z).
//  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data)

    val bricks = in
      .map { l =>
        val Array(start, end) = l.split("~").map { cs =>
          val Array(x, y, z) = cs.split(",")
          Coord3d(x.toInt, y.toInt, z.toInt)
        }
        Brick(start, end)
      }
      .sortBy(_.min.z)

    val dropped = bricks.tail
      .foldLeft(State(bricks.take(1), bricks.head.cubes)) {
        case (State(bricks, cubes), thisBrick) =>
          val drop = ns
            .find(n => {
              val droppedCubes = thisBrick.drop(n).cubes

              droppedCubes
                .exists(_.z <= 0) || droppedCubes.intersect(cubes).nonEmpty
            })
            .get - 1
          val droppedBrick = thisBrick.drop(drop)
          State(bricks :+ droppedBrick, cubes ++ droppedBrick.cubes)
      }
      .bs

    val bricksByFloor = dropped.groupBy(_.min.z)
    val bricksByRoof = dropped.groupBy(_.max.z)

    val p1 = dropped.filter { brick =>
      val roof = brick.max.z
      val nbors = bricksByRoof(roof)
      val supports = bricksByFloor.getOrElse(roof + 1, Seq.empty).filter(
        _.drop(1).cubes.intersect(brick.cubes).nonEmpty
      )
      supports.forall { supported =>
        val howManyNeighboursSupport = nbors.count { nbor =>
          supported.drop(1).cubes.intersect(nbor.cubes).nonEmpty
        }
        howManyNeighboursSupport > 1
      }
    }

    println(p1.size)

    val p2Cands = dropped.diff(p1)

    val allCubes = bricks.flatMap(_.cubes).toSet

    val x = p2Cands.sortBy(_.max.z).reverse.foldLeft(Map.empty[Brick, Seq[Brick]])((acc, brick) => {
      println(brick)
      val falling = dislodge(allCubes.diff(brick.cubes), dropped.filter(b => b.min.z > brick.max.z).toList, acc, Seq.empty)
      acc + ((brick, falling))
    }).map(_._2.size).sum

    val p2 = x
    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
