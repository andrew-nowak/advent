package lib

import scala.io.Source

trait Support {
  val newline = "\\r?\\n"

  def loadDirty: String =
    Source.fromResource("input.txt").mkString

  def load: String = loadDirty.trim

  def stringSeq(input: String, delimiter: String): Seq[String] =
    input.split(delimiter)
  def loadStringSeq(delimiter: String): Seq[String] =
    stringSeq(load, delimiter)

  def stringSeq(input: String): Seq[String] = stringSeq(input, newline)
  def loadStringSeq: Seq[String] = stringSeq(load)

  def intSeq(input: String, delimiter: String = newline): Seq[Int] =
    input
      .split(delimiter)
      .filter(_ != "")
      .map(_.toInt)
  def loadIntSeq(delimiter: String = newline): Seq[Int] =
    intSeq(load, delimiter)
  @deprecated
  def loadIntSeq: Seq[Int] = loadIntSeq()

  def `2dIntSeq`(input: String, delimiterA: String = newline, delimiterB: String = " "): Seq[Seq[Int]] =
    input
      .split(delimiterA)
      .filter(_ != "")
      .map(_.split(delimiterB).map(_.toInt).toSeq)
  def load2dIntSeq(delimiterA: String = newline, delimiterB: String = " "): Seq[Seq[Int]] =
    `2dIntSeq`(load, delimiterA, delimiterB)
  @deprecated def load2dIntSeq: Seq[Seq[Int]] = load2dIntSeq()

  def `2dIntSeqWithCoords`(input: String, delimiterA: String = newline, delimiterB: String = " "): Map[Coord, Int] =
    `2dIntSeq`(input, delimiterA, delimiterB).zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (i, x) => Coord(x, y) -> i }
    }.toMap
  def load2dIntSeqWithCoords(delimiterA: String = newline, delimiterB: String = " "): Map[Coord, Int] =
    `2dIntSeqWithCoords`(load, delimiterA, delimiterB)

}
