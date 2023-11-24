package twentyone.one

import lib.Support

object One extends App with Support {
  val i = loadIntSeq

  println(i.sliding(2, 1).count(w => w.head < w.last))

  println(i.sliding(3, 1).map(_.sum).sliding(2, 1).count(w => w.head < w.last))
}
