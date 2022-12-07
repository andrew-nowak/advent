package twentytwo.seven

import lib.Support

import scala.collection.mutable

case class File(name: String, size: Long)
case class Directory(name: String, subdirs: mutable.ListBuffer[Directory], contents: mutable.ListBuffer[File]) {
  def size: Long = contents.map(_.size).sum + subdirs.map(_.size).sum
  def undersize: Long =
    (if (size <= 100000) size else 0L) + subdirs.map(_.undersize).sum

  def bestchoice(to: Long): Option[Long] =
    (subdirs.map(_.bestchoice(to)) :+ (if (size >= to) Some(size) else None))
      .flatten.minOption
}
case class State(loc: String, currentDirectory: Directory, parents: Seq[Directory])
object TwentytwoSeven extends App with Support {
  val input = load

  def run(data: String) = {
    val in = stringSeq(data)

    val root = Directory("/", mutable.ListBuffer.empty, mutable.ListBuffer.empty)

    val initial = State("/", root, Seq.empty)

    val p1 = in.tail.foldLeft(initial)((fs, outp) => {
      if (outp == "$ ls") {
        fs
      } else if (outp == "$ cd ..") {
        fs.copy(currentDirectory = fs.parents.head, parents = fs.parents.tail)
      } else if (outp startsWith "$ cd ") {
        fs.copy(currentDirectory =
          fs.currentDirectory.subdirs.find(_.name == outp.drop(5)).get,
          parents = fs.currentDirectory +: fs.parents
        )
      } else if (outp startsWith "dir ") {
        val d = Directory(outp.drop(4), mutable.ListBuffer.empty, mutable.ListBuffer.empty)
        fs.currentDirectory.subdirs.addOne(d)
        fs
      } else { // file
        val Array(size, fname) = outp.split(" ")
        val f = File(fname, size.toLong)
        fs.currentDirectory.contents.addOne(f)
        fs
      }
    })

    val r = p1.parents.last
    println(p1.parents.last.undersize)

    val avail = 70000000 - r.size
    val toreclaim = 30000000 - avail
    println(r.bestchoice(toreclaim))
  }

  println("--- real ---")
  run(input)
}
