package twentyone.sixteen

import lib.Support

import scala.annotation.tailrec

final case class ParseResult[+T](value: T, unconsumedInput: List[String])

sealed trait Packet {
  val version: Int
  def getTotalVersion: Int
  def value: Long
}
object Packet {
  @tailrec
  def parseByCount(remaining: List[String], count: Int, parsed: List[Packet] = Nil): ParseResult[List[Packet]] = {
    if (count <= 0) ParseResult(parsed, remaining)
    else {
      val ParseResult(packet, rest) = parse(remaining)
      parseByCount(rest, count - 1, parsed :+ packet)
    }
  }

  @tailrec
  def parseUntilConsumed(remaining: List[String], parsed: List[Packet] = Nil): ParseResult[List[Packet]] = {
    if (remaining.isEmpty) {
      ParseResult(parsed, remaining)
    } else {
      val parseResult = parse(remaining)
      val allParsed = parsed :+ parseResult.value
      parseUntilConsumed(parseResult.unconsumedInput, allParsed)
    }
  }

  def parse(packet: List[String]): ParseResult[Packet] = {
    val version = Integer.parseInt(packet.take(3).mkString, 2)
    val typeId = Integer.parseInt(packet.slice(3, 6).mkString, 2)
    typeId match {
      case 4 => LiteralPacket.parse(version, packet.drop(6))
      case 0 => OperatorPacket.parse(version, packet.drop(6), _.sum)
      case 1 => OperatorPacket.parse(version, packet.drop(6), _.product)
      case 2 => OperatorPacket.parse(version, packet.drop(6), _.min)
      case 3 => OperatorPacket.parse(version, packet.drop(6), _.max)
      case 5 => OperatorPacket.parse(version, packet.drop(6), OperatorPacket.gt)
      case 6 => OperatorPacket.parse(version, packet.drop(6), OperatorPacket.lt)
      case 7 => OperatorPacket.parse(version, packet.drop(6), OperatorPacket._eq)

    }
  }
}

case class LiteralPacket(version: Int, value: Long) extends Packet {
  override def getTotalVersion: Int = version
}
object LiteralPacket {
  def parse(version: Int, input: List[String]): ParseResult[LiteralPacket] = {
    val (nibbles, unconsumed) = input.grouped(5).span(_.head == "1") match {
      case (head, tail) => ((head ++ tail.take(1)).toList, tail.toList)
    }
    val content = java.lang.Long.parseLong(nibbles.flatMap(_.tail).mkString, 2)
    val packet = LiteralPacket(version, content)
    ParseResult(packet, unconsumed.flatten)
  }
}

case class OperatorPacket(version: Int, subpackets: List[Packet], operation: List[Long] => Long) extends Packet {
  override def getTotalVersion: Int = version + subpackets.map(_.getTotalVersion).sum

  override def value: Long = operation(subpackets.map(_.value))

}
object OperatorPacket {
  def parse(version: Int, input: List[String], operation: List[Long] => Long): ParseResult[OperatorPacket] = {
    val lengthType = input.head
    lengthType match {
      case "0" => byBitLength(version, input.tail, operation)
      case "1" => bySubpacketCount(version, input.tail, operation)
    }
  }
  def byBitLength(version: Int, input: List[String], operation: List[Long] => Long): ParseResult[OperatorPacket] = {
    val (lengthBits, notLengthBits) = input.splitAt(15)
    val length = Integer.parseInt(lengthBits.mkString, 2)
    val (rawSubpackets, remaining) = notLengthBits.splitAt(length)
    val subpackets = Packet.parseUntilConsumed(rawSubpackets)
    require(subpackets.unconsumedInput.isEmpty)
    val packet = OperatorPacket(version, subpackets.value, operation)
    ParseResult(packet, remaining)
  }
  def bySubpacketCount(
      version: Int,
      input: List[String],
      operation: List[Long] => Long
  ): ParseResult[OperatorPacket] = {
    val (countBits, notCountBits) = input.splitAt(11)
    val count = Integer.parseInt(countBits.mkString, 2)
    val subpackets = Packet.parseByCount(notCountBits, count)
    val packet = OperatorPacket(version, subpackets.value, operation)
    ParseResult(packet, subpackets.unconsumedInput)
  }

  def gt(values: List[Long]): Long = if (values.head > values(1)) 1 else 0
  def lt(values: List[Long]): Long = if (values.head < values(1)) 1 else 0
  def _eq(values: List[Long]): Long = if (values.head == values(1)) 1 else 0
}

object TwentyoneSixteen extends App with Support {
  val hex = load
  val bin = hex
    .split("")
    .map(nibble => {
      val b = Integer.parseInt(nibble, 16).toBinaryString
      val lpad = Seq.fill(4 - b.length)("0").mkString
      s"$lpad$b"
    })
    .mkString

  val packet = Packet.parse(bin.split("").toList)

  println(packet.value.getTotalVersion)
  println(packet.value.value)
}
