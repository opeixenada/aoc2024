import PlutonianPebbles.{Pebble, process}
import Util.readFile

import scala.annotation.tailrec

@main def day11(): Unit = {
  val input = readFile("resources/day11")
  val solver = PlutonianPebbles(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object PlutonianPebbles {

  /** Pebble with a numerical value and remaining transformations count.
    *
    * @param value
    *   The numerical value of the pebble
    * @param blinks
    *   The number of remaining transformations (blinks) the pebble can undergo
    */
  case class Pebble(value: BigInt, blinks: Int) {

    /** Expands (evaluates) the pebble based on transformation rules. */
    def expand(): List[Pebble] = {
      lazy val str = value.toString()
      lazy val n = str.length

      val newValues = value match
        case _ if value == BigInt(0) => List(BigInt(1))
        case _ if n % 2 == 0 =>
          val (a, b) = str.splitAt(n / 2)
          List(a, b).map(BigInt.apply)
        case _ => List(value * 2024)

      newValues.map(Pebble(_, blinks - 1))
    }
  }

  /** Memoized partial evaluation results.
    *
    * @param expanded
    *   Pebbles and their expanded (evaluated) states for intermediate transformations
    * @param reduced
    *   Pebbles and their final numerical count after all transformations
    */
  case class Memory(
      expanded: Map[Pebble, List[Pebble]] = Map.empty,
      reduced: Map[Pebble, BigInt] = Map.empty
  )

  /** Generates intermediate memory entries for a pebble and its expansions. */
  private def getExpandedEntries(pebble: Pebble, expanded: List[Pebble]): List[(Pebble, List[Pebble])] =
    (0 until pebble.blinks).map { i =>
      val key = pebble.copy(blinks = pebble.blinks - i)
      val value = expanded.map(_.copy(blinks = pebble.blinks - i - 1))
      key -> value
    }.toList

  /** Updates the memory with new entries. */
  private def updateMemory(memory: Memory, entries: List[(Pebble, List[Pebble])]): Memory =
    entries.foldLeft(memory) { case (acc, (pebble, expandedPebbles)) =>
      if (pebble.blinks == 1) Memory(acc.expanded, acc.reduced + (pebble -> BigInt(expandedPebbles.length)))
      else Memory(acc.expanded + (pebble -> expandedPebbles), acc.reduced)
    }

  /** Recursively reduces the memory by converting expanded entries to their final values. */
  @tailrec
  private def reduceMemory(memory: Memory): Memory = memory.expanded.find { case (_, expandedPebbles) =>
    expandedPebbles.forall(memory.reduced.contains)
  } match
    case Some((pebble, expandedPebbles)) => // Something to reduce!
      val newReduced = memory.reduced + (pebble -> expandedPebbles.map(memory.reduced).sum)
      val newExpanded = memory.expanded - pebble
      reduceMemory(Memory(newExpanded, newReduced))
    case None => memory // Nothing to reduce

  /** Processes a list of pebbles through their transformations to compute final count. */
  @tailrec
  def process(
      states: List[Pebble],
      memory: Memory = Memory(),
      acc: BigInt = BigInt(0)
  ): BigInt = states match
    case Nil                      => acc
    case x :: xs if x.blinks == 0 => process(xs, memory, acc + 1)
    case x :: xs =>
      memory.reduced.get(x) match
        case Some(v) => process(xs, memory, acc + v)
        case None =>
          val nextStates = x.expand()
          process(
            nextStates ::: xs,
            reduceMemory(updateMemory(memory, getExpandedEntries(x, nextStates))),
            acc
          )
}

class PlutonianPebbles(input: List[String]) {

  private val values: List[BigInt] = input.head.split(' ').map(BigInt.apply).toList

  def solvePart1(): Any = process(values.map(Pebble(_, 25)))
  def solvePart2(): Any = process(values.map(Pebble(_, 75)))
}
