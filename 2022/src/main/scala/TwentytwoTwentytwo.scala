package twentytwo.twentytwo

import lib.Support
import lib.Coord
import lib.Direction
import lib.SeqExtras
import scala.annotation.tailrec

case class Position(location: Coord, direction: Direction)
case class Board(map: Map[Coord, Boolean], rows: Seq[(Int, Int)], cols: Seq[(Int, Int)])

object TwentytwoTwentytwo extends App with Support {
  val testData =
    """        ...#
      |        .#..
      |        #...
      |        ....
      |...#.......#
      |........#...
      |..#....#....
      |..........#.
      |        ...#....
      |        .....#..
      |        .#......
      |        ......#.
      |
      |10R5L5R10L4R5L5
      |""".stripMargin
  val input = loadDirty

  def getStartEnds(ms: Map[Int, Iterable[Int]]): Seq[(Int, Int)] = {
    ms.toSeq.sortBy(_._1).map(_._2.toSeq).map(ns => ns.min -> (ns.max - ns.min))
  }

  def resolve(initial: Int, constraints: (Int, Int), increment: Int): Int = {
    val normalised = initial - constraints._1 + increment
    val decycled =
      if (normalised < 0) (normalised % constraints._2 + 1) + constraints._2
      else normalised % (constraints._2 + 1)
    decycled + constraints._1
  }

  /*
          1122
          1122
          33
          33
        5544
        5544
        66
        66
   */

  def test() = {
    require(
      resolveP2(Position(Coord(70, 0), Direction.Up)) == Position(Coord(0, 170), Direction.Right)
    )
    require(
      resolveP2(Position(Coord(120, 0), Direction.Up)) == Position(Coord(20, 199), Direction.Up)
    )
    require(
      resolveP2(Position(Coord(149, 20), Direction.Right)) == Position(Coord(99, 129), Direction.Left)
    )
    require(resolveP2(Position(Coord(120, 49), Direction.Down)) == Position(Coord(99, 70), Direction.Left))
    require(resolveP2(Position(Coord(99, 70), Direction.Right)) == Position(Coord(120, 49), Direction.Up))
    require(resolveP2(Position(Coord(99, 129), Direction.Right)) == Position(Coord(149, 20), Direction.Left))
    require(resolveP2(Position(Coord(70, 149), Direction.Down)) == Position(Coord(49, 170), Direction.Left))
    require(resolveP2(Position(Coord(49, 170), Direction.Right)) == Position(Coord(70, 149), Direction.Up))
    require(resolveP2(Position(Coord(20, 199), Direction.Down)) == Position(Coord(120, 0), Direction.Down))
    require(resolveP2(Position(Coord(0, 170), Direction.Left)) == Position(Coord(70, 0), Direction.Down))
    require(resolveP2(Position(Coord(0, 120), Direction.Left)) == Position(Coord(50, 29), Direction.Right))
    require(resolveP2(Position(Coord(20, 100), Direction.Up)) == Position(Coord(50, 70), Direction.Right))
    // rest missing
  }

  def resolveP2(from: Position): Position = {
    from match {
      case Position(Coord(149, y), Direction.Right) if y < 50 =>
        Position(Coord(99, 149 - y), Direction.Left) // right off 2 -> 4
      case Position(Coord(x, 49), Direction.Down) if x >= 100 =>
        Position(Coord(99, x - 50), Direction.Left) // down off 2 -> 3
      case Position(Coord(99, y), Direction.Right) if y >= 50 && y < 100 =>
        Position(Coord(y + 50, 49), Direction.Up) // right off 3 -> 2
      case Position(Coord(99, y), Direction.Right) if y >= 100 && y < 150 =>
        Position(Coord(149, 149 - y), Direction.Left) // right off 4 -> 2
      case Position(Coord(x, 149), Direction.Down) if x >= 50 && x < 100 =>
        Position(Coord(49, x + 100), Direction.Left) // down off 4 -> 6
      case Position(Coord(49, y), Direction.Right) if y >= 150 =>
        Position(Coord(y - 100, 149), Direction.Up) // right off 6 -> 4
      case Position(Coord(x, 199), Direction.Down) if x < 50 =>
        Position(Coord(x + 100, 0), Direction.Down) // down off 6 -> 2
      case Position(Coord(0, y), Direction.Left) if y >= 150 =>
        Position(Coord(y - 100, 0), Direction.Down) // left off 6 -> 1
      case Position(Coord(0, y), Direction.Left) if y >= 100 =>
        Position(Coord(50, 149 - y), Direction.Right) // left off 5 -> 1       // 149 => 0, 100 => 49
      case Position(Coord(x, 100), Direction.Up) if x < 50 =>
        Position(Coord(50, x + 50), Direction.Right) // up off 5 -> 3  // 0 => 49, 49 => 99
      case Position(Coord(50, y), Direction.Left) if y >= 50 && y < 100 =>
        Position(Coord(y - 50, 100), Direction.Down) // left off 3 -> 5 // 50 => 0, 99 => 49
      case Position(Coord(50, y), Direction.Left) if y < 50 =>
        Position(Coord(0, 149 - y), Direction.Right) // 0 => 149, 49 => 100
      case Position(Coord(x, 0), Direction.Up) if x < 100 =>
        Position(Coord(0, x + 100), Direction.Right) // up off 1 -> 6
      case Position(Coord(x, 0), Direction.Up) if x >= 100 =>
        Position(Coord(x - 100, 199), Direction.Up) // up off 2 -> 6

      case Position(Coord(x, y), Direction.Up)    => Position(Coord(x, y - 1), Direction.Up)
      case Position(Coord(x, y), Direction.Down)  => Position(Coord(x, y + 1), Direction.Down)
      case Position(Coord(x, y), Direction.Left)  => Position(Coord(x - 1, y), Direction.Left)
      case Position(Coord(x, y), Direction.Right) => Position(Coord(x + 1, y), Direction.Right)
    }
  }

  @tailrec def moveP2(pos: Position, board: Board, instruction: List[Char]): Position = instruction match {
    case Nil         => pos
    case 'L' :: rest => moveP2(pos.copy(direction = pos.direction.ccw), board, rest)
    case 'R' :: rest => moveP2(pos.copy(direction = pos.direction.cw), board, rest)
    case instrs =>
      val (distraw, rest) = instrs.span(_.isDigit)
      val dist = distraw.mkString.toInt
      val steps = SeqExtras.produce(1 to dist)(pos)((prev, _) => resolveP2(prev))

      val nextposition = steps
        .takeWhile(step => { /*println(step);*/
          board.map(step.location)
        })
        .lastOption
        .getOrElse(pos)
      moveP2(nextposition, board, rest)
  }
  @tailrec def move(pos: Position, board: Board, instruction: List[Char]): Position = {
    instruction match {
      case Nil         => pos
      case 'L' :: rest => move(pos.copy(direction = pos.direction.ccw), board, rest)
      case 'R' :: rest => move(pos.copy(direction = pos.direction.cw), board, rest)
      case instrs =>
        val (distraw, rest) = instrs.span(_.isDigit)
        val dist = distraw.mkString.toInt
        val steps = (1 to dist).map { n =>
          pos.direction match {
            case Direction.Up =>
              val col = board.cols(pos.location.x)
              Coord(pos.location.x, resolve(pos.location.y, col, -n))
            case Direction.Down =>
              val col = board.cols(pos.location.x)
              Coord(pos.location.x, resolve(pos.location.y, col, n))
            case Direction.Left =>
              val row = board.rows(pos.location.y)
              Coord(resolve(pos.location.x, row, -n), pos.location.y)
            case Direction.Right =>
              val row = board.rows(pos.location.y)
              Coord(resolve(pos.location.x, row, n), pos.location.y)
          }
        }
        val location = steps.takeWhile(board.map).lastOption.getOrElse(pos.location)
        move(pos.copy(location = location), board, rest)
    }
  }

  val directionScore: Direction => Int = {
    case Direction.Right => 0
    case Direction.Down  => 1
    case Direction.Left  => 2
    case Direction.Up    => 3
  }

  def run(data: String) = {
    val Array(rawmap, instructions) = data.split(newline + newline)

    val map = rawmap
      .split(newline)
      .filter(_.nonEmpty)
      .zipWithIndex
      .flatMap { case (row, y) =>
        row.zipWithIndex.collect {
          case ('.', x) => Coord(x, y) -> true
          case ('#', x) => Coord(x, y) -> false
        }
      }
      .toMap

    val rows: Seq[(Int, Int)] = getStartEnds(map.keys.groupMap(_.y)(_.x))
    val cols = getStartEnds(map.keys.groupMap(_.x)(_.y))

    val board = Board(map, rows, cols)

    val start = Position(Coord(board.rows.head._1, 0), Direction.Right)

    printCoords(
      board.map.map(n => n._1 -> (if (n._1 == start.location) 2 else if (n._2) 1 else 0)),
      {
        case 2 => 'X'
        case 1 => '.'
        case _ => '#'
      }
    )

    val p1 = move(start, board, instructions.trim.toCharArray().toList)

    printCoords(
      board.map.map(n => n._1 -> (if (n._1 == p1.location) 2 else if (n._2) 1 else 0)),
      {
        case 2 => 'X'
        case 1 => '.'
        case 0 => '#'
      }
    )

    def score(p: Position): Int = 1000 * (p.location.y + 1) + 4 * (p.location.x + 1) + directionScore(p.direction)

    println(score(p1))

    test()

    val p2 = moveP2(start, board, instructions.trim().toCharArray().toList)
    println(score(p2))
  }

  println("--- testdata ---")
  // run(testData)
  println("--- real ---")
  run(input)
}
