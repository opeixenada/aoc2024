import Util.readFile
import WarehouseWoes.{Coordinates, Warehouse, boxesSum, process, rearrange}

import scala.annotation.tailrec

@main def day15(): Unit = {
  val input = readFile("resources/day15")
  val solver = WarehouseWoes(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object WarehouseWoes {
  type Coordinates = (Int, Int)
  type Warehouse = List[String]

  @tailrec
  private def moveBox(xs: String, movedBoxes: Int = 0): Option[String] = xs.head match
    case '#' => None
    case '.' => Some(List.fill(movedBoxes)('O').mkString + xs.tail)
    case 'O' => moveBox(xs.tail, movedBoxes + 1)

  def rearrange(xs: String): String = {
    val prefix = xs.takeWhile(_ != '@')
    val rest = xs.drop(prefix.length + 1)
    rest.head match
      case '.' => prefix + ".@" + rest.drop(1)
      case '#' => xs
      case 'O' =>
        moveBox(rest) match
          case Some(suffix) => prefix + ".@" + suffix
          case _            => xs
  }

  def transposeCoordinates(cs: Coordinates): Coordinates = cs.swap

  @tailrec
  def transpose(map: Warehouse, result: Warehouse = Nil): Warehouse = map.headOption.flatMap(_.headOption) match
    case None => result
    case x    => transpose(map.map(_.tail), result.appended(map.map(_.head).mkString))

  def turnBack(map: Warehouse): Warehouse = (0 until 3).foldLeft(map) { case (m, _) => WarehouseWoes.transpose(m) }

  def move(state: (Coordinates, Warehouse), instruction: Char): (Coordinates, Warehouse) = {
    val (robot, map) = state

    def handleHorizontalMove(isRightward: Boolean): (Coordinates, Warehouse) = {
      val currentRow = map.drop(robot._2).head
      val rearranged =
        if (isRightward) rearrange(currentRow)
        else rearrange(currentRow.reverse).reverse

      // Here instead to getting the robot's coordinates (linear) we could pass it from the `rearrange` function.
      (rearranged.indexOf('@') -> robot._2) -> (map.take(robot._2) ::: (rearranged :: map.drop(robot._2 + 1)))
    }

    def handleVerticalMove(isDownward: Boolean): (Coordinates, Warehouse) = {
      val transposed = transpose(map)
      val currentColumn = transposed.drop(robot._1).head
      val rearranged =
        if (isDownward) rearrange(currentColumn)
        else rearrange(currentColumn.reverse).reverse
      val resultTransposed = transposed.take(robot._1) ::: (rearranged :: transposed.drop(robot._1 + 1))

      // Here instead of turning the whole map to the original orientation we could memorize that the orientation is
      // transposed, and transpose all the instructions instead. That would save us up to 3n transpositions.
      (robot._1 -> rearranged.indexOf('@')) -> turnBack(resultTransposed)
    }

    instruction match {
      case '>' => handleHorizontalMove(isRightward = true)
      case '<' => handleHorizontalMove(isRightward = false)
      case 'v' => handleVerticalMove(isDownward = true)
      case '^' => handleVerticalMove(isDownward = false)
      case ch  => throw new IllegalArgumentException(s"Invalid instruction: $ch")
    }
  }

  def process(robot: Coordinates, map: Warehouse, instructions: List[Char]): (Coordinates, Warehouse) =
    instructions.foldLeft(robot -> map)(move)

  def boxesSum(map: Warehouse): Int = (for {
    (line, y) <- map.zipWithIndex
    (ch, x) <- line.zipWithIndex
    if ch == 'O' || ch == '['
  } yield 100 * y + x).sum
}

class WarehouseWoes(input: List[String]) {

  val map: Warehouse = input.takeWhile(!_.isBlank)
  val instructions: List[Char] = input.drop(map.length + 1).flatten.filter(Set('<', '^', '>', 'v').contains)

  val robot: Coordinates = (for {
    y <- map.indices
    x <- map.head.indices
    if map(y)(x) == '@'
  } yield (x, y)).head

  def solvePart1(): Any = boxesSum(process(robot, map, instructions)._2)
  def solvePart2(): Any = ???
}
