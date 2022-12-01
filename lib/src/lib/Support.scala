package lib

import scala.io.Source

trait Support {
  val newline = "\\r?\\n"

  def loadDirty: String =
    Source.fromResource("input.txt").mkString

  def load: String = loadDirty.trim

  def loadStringSeq(delimiter: String): Seq[String] =
    load.split(delimiter)

  def loadStringSeq: Seq[String] = loadStringSeq(newline)

  def loadIntSeq(delimiter: String = newline): Seq[Int] =
    load
      .split(delimiter)
      .filter(_ != "")
      .map(_.toInt)

  def loadIntSeq: Seq[Int] = loadIntSeq()

  def load2dIntSeq(delimiterA: String = newline, delimiterB: String = " "): Seq[Seq[Int]] =
    load
      .split(delimiterA)
      .filter(_ != "")
      .map(_.split(delimiterB).map(_.toInt).toSeq)

  def load2dIntSeq: Seq[Seq[Int]] = load2dIntSeq()

  def load2dIntSeqWithCoords(delimiterA: String = newline, delimiterB: String = " "): Map[Coord, Int] =
    load2dIntSeq(delimiterA, delimiterB).zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (i, x) => Coord(x, y) -> i }
    }.toMap
}
