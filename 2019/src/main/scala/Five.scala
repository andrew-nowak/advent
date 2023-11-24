package nineteen.five

import lib.Intcode
import lib.Support

object Five extends App with Support {
  println(Intcode.buildAndRunWithIo(loadLongSeq(","), input = List(1)).last)
  println(Intcode.buildAndRunWithIo(loadLongSeq(","), input = List(5)).last)
}
