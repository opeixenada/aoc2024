import LinenLayout.{countArrangements, isPossible}
import Util.readFile

import scala.annotation.tailrec

@main def day19(): Unit = {
  val input = readFile("resources/day19")
  val solver = LinenLayout(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object LinenLayout {
  def isPossible(patterns: List[String])(design: String): Boolean = {
    @tailrec
    def check(designs: List[String]): Boolean = designs match
      case Nil                 => false
      case d :: _ if d.isEmpty => true
      case d :: ds =>
        check(patterns.filter(d.startsWith).map { prefix =>
          d.drop(prefix.length)
        } ::: ds)

    check(List(design))
  }

  def countArrangements(patterns: List[String])(design: String): Int = {
    @tailrec
    def count(designs: List[String], acc: Int = 0): Int = designs match
      case Nil => acc
      case d :: ds =>
        count(
          designs = patterns.filter(d.startsWith).map { prefix =>
            d.drop(prefix.length)
          } ::: ds,
          acc = if (d.isEmpty) acc + 1 else acc
        )

    count(List(design))
  }
}

class LinenLayout(input: List[String]) {

  private val patterns = input.head.split(", ").toList
  private val designs = input.drop(2)

  def solvePart1(): Any = designs.count(isPossible(patterns))
  def solvePart2(): Any = designs.map(countArrangements(patterns)).sum
}
