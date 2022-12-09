package twentytwo.seven

import lib.Support

object TwentytwoSeven extends App with Support {
  val input = load

  case class State(cwd: Seq[String], dirs: Map[String, Int]) {
    def pwds: Seq[String] = "/" +: cwd.indices.map(n => cwd.take(n + 1)).map(_.mkString("/", "/", ""))
  }

  def run(data: String) = {
    val in = stringSeq(data)

    val initial = State(Seq(), Map.empty)

    val cdUp = """\$ cd ..""".r
    val cd = """\$ cd (.+)""".r
    val file = """(\d+) (.+)""".r
    val tree = in.tail.foldLeft(initial)((fs, instr) => {
      instr match {
        case cdUp()  => State(fs.cwd.dropRight(1), fs.dirs)
        case cd(dir) => State(fs.cwd :+ dir, fs.dirs)
        case file(size, _) =>
          fs.copy(dirs =
            fs.pwds.foldLeft(fs.dirs)((dirMap, dirname) =>
              dirMap.updatedWith(dirname)(_.map(_ + size.toInt).orElse(Some(size.toInt)))
            )
          )
        case _ => fs // $ls and dir entries do nothing
      }
    })

    val p1 = tree.dirs.values.filter(_ <= 100000).sum
    println(p1)

    val tot = 30000000 - (70000000 - tree.dirs("/"))
    val p2 = tree.dirs.values.filter(_ >= tot).min
    println(p2)
  }

  run(input)
}
