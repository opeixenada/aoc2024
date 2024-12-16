import Util.readFile
import WarehouseWoes.{boxesSum, parseExpandedInput, parseInput, process, processLineForBigBoxes}

import scala.annotation.tailrec

@main def day15(): Unit = {
  val input = readFile("resources/day15")
  val solver = WarehouseWoes(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object WarehouseWoes {
  private type Grid = List[List[Char]]

  /**   - Current character;
    *   - Previous character (will be used if the current one will move);
    *   - Flag indicating if the character is moving.
    */
  private type MovingLine = (Char, Char, Boolean)

  private type LineProcessor = (List[MovingLine], Char) => List[MovingLine]

  def parseInput(input: List[String]): (Grid, List[Char]) = {
    val map: Grid = input.takeWhile(!_.isBlank).map(_.toCharArray.toList)
    val instructions: List[Char] = input.drop(map.length + 1).flatten.filter(Set('<', '^', '>', 'v').contains)
    map -> instructions
  }

  def parseExpandedInput(input: List[String]): (Grid, List[Char]) = {
    val (map, instructions) = parseInput(input)

    val expandedMap = map.map {
      _.flatMap {
        case '#'   => "##"
        case 'O'   => "[]"
        case '.'   => ".."
        case '@'   => "@."
        case other => throw Exception(s"Invalid grid symbol: $other")
      }
    }

    expandedMap -> instructions
  }

  def process(input: (Grid, List[Char]), processLine: LineProcessor = processLineIdentity): Grid = {
    val (map: Grid, instructions: List[Char]) = input
    instructions.foldLeft(map)(move(processLine))
  }

  def move(processLine: LineProcessor = processLineIdentity)(map: Grid, instruction: Char): Grid = instruction match {
    case 'v' => moveDown(processLine)(map, 'v')
    case '^' => moveDown(processLine)(map.reverse, '^').reverse
    case '>' => moveDown(processLine)(map.transpose, '>').transpose
    case '<' => moveDown(processLine)(map.transpose.reverse, '<').reverse.transpose
    case ch  => throw new IllegalArgumentException(s"Invalid instruction: $ch")
  }

  private def moveDown(processLine: LineProcessor)(map: Grid, command: Char): Grid = {
    val prefix = map.takeWhile(!_.contains('@'))
    val movingLine = map.drop(prefix.length).head.map {
      case '@' => ('@', '.', true)
      case ch  => (ch, '.', false)
    }
    moveLineDown(processLine)(prefix, movingLine, map.drop(prefix.length + 1), command).getOrElse(map)
  }

  private def processLineIdentity(line: List[MovingLine], command: Char): List[MovingLine] = line

  /** Moves the whole box if one side of it is moving. */
  def processLineForBigBoxes(line: List[MovingLine], command: Char): List[MovingLine] =
    command match
      case '>' | '<' => line
      case _ =>
        line.head ::
          line
            .zip(line.tail)
            .zip(line.tail.tail)
            .map { case ((left, x), right) =>
              if (left._1 == '[' && x._1 == ']' && !x._3 && left._3) (x._1, '.', true)
              else if (x._1 == '[' && right._1 == ']' && !x._3 && right._3) (x._1, '.', true)
              else x
            }
            .appended(line.last)

  @tailrec
  private def moveLineDown(
      processLine: LineProcessor
  )(prefix: Grid, line: List[MovingLine], suffix: Grid, command: Char): Option[Grid] = suffix match
    case Nil => Some(prefix.appended(line.map(_._1)))
    case s :: tail =>
      val nextLines: List[Option[(Char, (Char, Char, Boolean))]] = line.zip(s).map {
        case ((ch1, ch0, false), ch2) =>
          // Character is not moving
          Some(ch1, (ch2, ch1, false))
        case ((ch1, ch0, true), '.') =>
          // Found a free spot! Stop moving
          Some(ch0, (ch1, '.', false))
        case ((ch1, ch0, true), '#') =>
          // Can't move
          None
        case ((ch1, ch0, true), ch2) =>
          // Another moveable character, move everything
          Some(ch0, (ch2, ch1, true))
      }

      if (nextLines.exists(_.isEmpty)) {
        // Can't move
        None
      } else {
        val movedLine = nextLines.flatMap(_.map(_._1))
        val nextLine = processLine(nextLines.flatMap(_.map(_._2)), command)
        moveLineDown(processLine)(prefix.appended(movedLine), nextLine, tail, command)
      }

  def boxesSum(map: Grid): Int = (for {
    (line, y) <- map.zipWithIndex
    (ch, x) <- line.zipWithIndex
    if ch == 'O' || ch == '['
  } yield 100 * y + x).sum
}

class WarehouseWoes(input: List[String]) {
  def solvePart1(): Any = boxesSum(process(parseInput(input)))
  def solvePart2(): Any = boxesSum(process(parseExpandedInput(input), processLineForBigBoxes))
}
