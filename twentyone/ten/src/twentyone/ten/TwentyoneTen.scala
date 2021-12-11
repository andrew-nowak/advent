package twentyone.ten

import lib.Support

import scala.annotation.tailrec

object TwentyoneTen extends App with Support {
  @tailrec def traverse(stack: List[Char], remaining: List[Char]): Either[Char, List[Char]] = {
    remaining match {
      case Nil                                => Right(stack)
      case '{' :: rest                        => traverse('}' :: stack, rest)
      case '<' :: rest                        => traverse('>' :: stack, rest)
      case '[' :: rest                        => traverse(']' :: stack, rest)
      case '(' :: rest                        => traverse(')' :: stack, rest)
      case head :: rest if head == stack.head => traverse(stack.tail, rest)
      case illegalChar :: _                   => Left(illegalChar)
    }
  }
  val scoreCorrupt: Char => Int = {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  def scoreIncomplete(remaining: List[Char]): Long =
    remaining.foldLeft(0L)((acc, missing) =>
      acc * 5 + (missing match {
        case ')' => 1
        case ']' => 2
        case '}' => 3
        case '>' => 4
      })
    )

  val i = loadStringSeq

  val (corrupt, incomplete) = i.partitionMap(line => traverse(Nil, line.toCharArray.toList))

  val part1 = corrupt.map(scoreCorrupt).sum
  println(part1)

  val part2 = incomplete.map(scoreIncomplete).sorted.apply(incomplete.size / 2)
  println(part2)
}
