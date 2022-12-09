package lib

import scala.annotation.tailrec
import scala.io.Source

trait Support {
  val newline = "\\r?\\n"

  def loadDirty: String =
    Source.fromResource("input.txt").mkString

  def load: String = loadDirty.trim

  def stringSeq(input: String, delimiter: String): Seq[String] =
    input.split(delimiter).toIndexedSeq
  def loadStringSeq(delimiter: String): Seq[String] =
    stringSeq(load, delimiter)

  def stringSeq(input: String): Seq[String] = stringSeq(input, newline)
  def loadStringSeq: Seq[String] = stringSeq(load)

  def intSeq(input: String, delimiter: String = newline): Seq[Int] =
    input
      .split(delimiter)
      .filter(_ != "")
      .map(_.toInt)
      .toIndexedSeq
  def loadIntSeq(delimiter: String = newline): Seq[Int] =
    intSeq(load, delimiter)
  @deprecated
  def loadIntSeq: Seq[Int] = loadIntSeq()

  def longSeq(input: String, delimiter: String = newline): Seq[Long] =
    input
      .split(delimiter)
      .filter(_ != "")
      .map(_.toLong)
      .toIndexedSeq

  def loadLongSeq(delimiter: String = newline): Seq[Long] =
    longSeq(load, delimiter)

  @deprecated
  def loadLongSeq: Seq[Long] = loadLongSeq()

  def `2dIntSeq`(input: String, delimiterA: String = newline, delimiterB: String = " "): Seq[Seq[Int]] =
    input
      .split(delimiterA)
      .filter(_ != "")
      .map(_.split(delimiterB).map(_.toInt).toSeq)
      .toIndexedSeq
  def load2dIntSeq(delimiterA: String = newline, delimiterB: String = " "): Seq[Seq[Int]] =
    `2dIntSeq`(load, delimiterA, delimiterB)
  @deprecated def load2dIntSeq: Seq[Seq[Int]] = load2dIntSeq()

  def `2dIntSeqWithCoords`(input: String, delimiterA: String = newline, delimiterB: String = " "): Map[Coord, Int] =
    `2dIntSeq`(input, delimiterA, delimiterB).zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (i, x) => Coord(x, y) -> i }
    }.toMap
  def load2dIntSeqWithCoords(delimiterA: String = newline, delimiterB: String = " "): Map[Coord, Int] =
    `2dIntSeqWithCoords`(load, delimiterA, delimiterB)

  @tailrec
  final def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def printCoords(s: Seq[Coord]) = {
    val horiz = s.map(_.x)
    val left = horiz.min
    val right = horiz.max
    val vert = s.map(_.y)
    val top = vert.min
    val bot = vert.max

    val cs = s.toSet

    for { y <- (top to bot).reverse } {
      val line = for { x <- left to right } yield {
        if (cs.contains(Coord(x, y))) '█' else ' '
      }
      println(line.mkString)
    }
  }
}
