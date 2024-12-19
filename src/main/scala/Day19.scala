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

  /** Counts all possible ways to build a target design using a given set of substring patterns. Uses recursive
    * backtracking with memoization.
    */
  def countArrangements(patterns: List[String])(design: String): BigInt = {

    /** Recursive helper that computes number of ways to build a remaining substring. Uses a cache to memoize results
      * for subproblems.
      *
      * This function doesn't need to be tail recursive despite the recursion because:
      *   1. The recursion depth is naturally limited by the input string length: each recursive call processes a
      *      shorter substring.
      *   1. Memoization ensures we never recalculate the same substring twice — once a result is cached, subsequent
      *      calls for that substring return immediately.
      */
    def count(remaining: String, memory: Map[String, BigInt] = Map.empty): (BigInt, Map[String, BigInt]) =
      memory.get(remaining) match
        case Some(result)           => (result, memory)
        case _ if remaining.isEmpty => (BigInt(1), memory)
        case _                      =>
          // For each pattern that could start our remaining string:
          // 1. Recursively solve for the rest of the string after removing the pattern
          // 2. Sum up all possible ways and accumulate cache updates
          val (result, newCache) = patterns
            .filter(remaining.startsWith)
            .foldLeft((BigInt(0), memory)) { case ((acc, currentCache), pattern) =>
              val (subCount, updatedCache) = count(remaining.drop(pattern.length), currentCache)
              (acc + subCount, updatedCache)
            }

          (result, newCache + (remaining -> result))

    count(design)._1
  }
}

class LinenLayout(input: List[String]) {

  private val patterns = input.head.split(", ").toList
  private val designs = input.drop(2)

  def solvePart1(): Any = designs.count(isPossible(patterns))
  def solvePart2(): Any = designs.map(countArrangements(patterns)).sum
}
