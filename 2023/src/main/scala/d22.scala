import lib._

object d22 extends App with Support {
  final class Brick(l: Coord3d, r: Coord3d) {
    val Seq(min, max) = Seq(l, r).sortBy(_.manhattan)
//    println(min, max)
    val cubes = for {
      x <- min.x to max.x
      y <- min.y to max.y
      z <- min.z to max.z
    } yield Coord3d(x, y, z)

    def drop(n: Int): Brick = {
      val lower = Coord3d(0, 0, -n)
      new Brick(l + lower, r + lower)
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

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data)

    val bricks = in
      .map { l =>
        val Array(start, end) = l.split("~").map { cs =>
          val Array(x, y, z) = cs.split(",")
          Coord3d(x.toInt, y.toInt, z.toInt)
        }
        new Brick(start, end)
      }
      .sortBy(_.min.z)

    val dropped = bricks.tail
      .foldLeft(State(bricks.take(1), bricks.head.cubes.toSet)) {
        case (State(bricks, cubes), thisBrick) =>
          val drop = ns
            .find(n => {
              val droppedCubes = thisBrick.drop(n).cubes.toSet

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
      val supports = bricksByFloor.getOrElse(roof + 1, Seq.empty).filter {
        _.drop(1).cubes.intersect(brick.cubes).nonEmpty
      }
      supports.forall { supported =>
        val howManyNeighboursSupport = nbors.count { nbor =>
          supported.drop(1).cubes.intersect(nbor.cubes).nonEmpty
        }
        howManyNeighboursSupport > 1
      }
    }

//    for { b <- bricks.reverse } println(b)
//    for { b <- dropped.reverse } println(b)

    println(p1.size)

    val p2 = in.size
//    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)
}
