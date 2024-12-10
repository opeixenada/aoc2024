import Util.readFile

import scala.annotation.tailrec

@main def day10(): Unit = {
  val input = readFile("resources/day10")
  val solver = HoofIt(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

class HoofIt(input: List[String]) {

  private val map: IndexedSeq[IndexedSeq[Char]] = input.map(_.toIndexedSeq).toIndexedSeq

  private val trailheads = for {
    x <- map.indices
    y <- map.head.indices
    if map(x)(y) == '0'
  } yield State(x, y, 0)

  case class State(x: Int, y: Int, value: Int) {
    def next(): List[State] = (for {
      (x2, y2) <- List((x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y))
      if x2 > -1 && y2 > -1 && x2 < map.length && y2 < map.head.length
      if map(x2)(y2) == value + 1 + '0'
    } yield State(x2, y2, value + 1)).toList
  }

  @tailrec
  private def getScore(xs: List[State], acc: Set[(Int, Int)] = Set.empty): Int = xs match
    case s :: tail if s.value == 9 => getScore(tail, acc + (s.x -> s.y))
    case s :: tail                 => getScore(s.next() ::: tail, acc)
    case _                         => acc.size

  def solvePart1(): Any = trailheads.map(t => getScore(List(t))).sum

  @tailrec
  private def getRating(xs: List[State], acc: Int = 0): Int = xs match
    case s :: tail if s.value == 9 => getRating(tail, acc + 1)
    case s :: tail                 => getRating(s.next() ::: tail, acc)
    case _                         => acc

  def solvePart2(): Any = trailheads.map(t => getRating(List(t))).sum

}
