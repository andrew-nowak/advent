import lib.Support

object d15 extends App with Support {
  sealed trait Tile

  object Rock extends Tile

  object Block extends Tile

  val testData =
    """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7""".trim.stripMargin
  val input = load

  def HASH(s: String): Int = {
    s.toCharArray.foldLeft(0)((acc, c) => {
      (acc + c.toInt) * 17 % 256
    })
  }

  final case class Box(lenses: Seq[(String, Int)] = Seq.empty)
  final case class State(boxes: Seq[Box] = Seq.fill(256)(Box()))

  def HASHMAP(s: State, step: String): State = {
    val (label, rest) = step.span(_.isLetter)
    val ibox = HASH(label)
    val box = s.boxes(ibox)

    val op = rest.head
    if (op == '-')
      s.copy(boxes =
        s.boxes.updated(ibox, Box(box.lenses.filterNot(_._1 == label)))
      )
    else {
      val focal = rest.tail.toInt
      val has = box.lenses.indexWhere(_._1 == label)
      s.copy(boxes =
        s.boxes.updated(
          ibox,
          Box(
            if (has == -1) box.lenses :+ (label, focal)
            else box.lenses.updated(has, (label, focal))
          )
        )
      )
    }
  }

  def run(data: String): Unit = {
    val start = System.nanoTime()

    val in = stringSeq(data, delimiter = ",")

    val p1 = in.map(HASH).sum

    val p2 = in
      .foldLeft(State())(HASHMAP)
      .boxes
      .zipWithIndex
      .flatMap { case (box, ibox) =>
        box.lenses.zipWithIndex.map { case ((_, focal), ilens) =>
          (1 + ibox) * (1 + ilens) * focal
        }
      }
      .sum

    println(p1)

    println(p2)

    val end = System.nanoTime()
    println(s"Done in ${(end - start).toDouble / 1_000_000} ms")
  }

  println("--- testdata ---")
  run(testData)
  println("--- real ---")
  run(input)

}
