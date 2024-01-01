import lib.{Coord, Support}

import scala.annotation.tailrec

object d20 extends App with Support {
  sealed trait Pixel {}
  object Pixel {
    def parse(c: Char): Pixel = c match {
      case '#' => Light
      case '.' => Dark
    }
  }
  case object Dark extends Pixel {
    override def toString: String = "0"
  }
  case object Light extends Pixel {
    override def toString: String = "1"
  }

  final case class Image(pixels: Map[Coord, Pixel], background: Pixel) {
    def enhance(alg: Seq[Pixel]): Image = {
      val toConsider = pixels.keySet.flatMap(_.surrounding)

      val nextBg = if (background == Dark) alg.head else alg.last

      val nextPixels = toConsider.foldLeft(Map.empty[Coord, Pixel])((generation, pixel) => {
        val binIndex = pixel.surrounding.map(loc => pixels.getOrElse(loc, background).toString).mkString
        val index = Integer.parseInt(binIndex, 2)
        require(index >= 0 && index <= 512)
        generation + (pixel -> alg(index))
      })
      Image(nextPixels, nextBg)
    }
  }

  object enhancer {
    @tailrec
    def enhance(alg: Seq[Pixel], image: Image, times: Int): Image = {
      if (times <= 0) image
      else enhance(alg, image.enhance(alg), times - 1)
    }
  }
  val i = loadStringSeq
  val alg = i.head map Pixel.parse
  val img = Image(
    pixels = i
      .drop(2)
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.map { case (ch, x) =>
          Coord(x, y) -> Pixel.parse(ch)
        }
      }
      .toMap,
    background = Dark
  )

  val part1 = enhancer.enhance(alg, img, times = 2)
  println(part1.pixels.count(_._2 == Light))

  val part2 = enhancer.enhance(alg, img, times = 50)
  println(part2.pixels.count(_._2 == Light))
}
