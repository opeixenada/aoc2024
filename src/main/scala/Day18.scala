import RAMRun.{State, findPath, h, w}
import Util.{Point, readFile}

import scala.annotation.tailrec

@main def day18(): Unit = {
  val input = readFile("resources/day18")
  val solver = RAMRun(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object RAMRun {

  val w: Int = 6
  val h: Int = 6

  def inBounds(p: Point): Boolean = p._1 > -1 && p._1 < w && p._2 > -1 && p._2 < h

  val directions: List[(Int, Int)] = List((0, 1), (0, -1), (1, 0), (-1, 0))

  case class State(
      bytes: List[Point],
      corrupt: Set[Point] = Set.empty,
      point: Point = (0, 0),
      seen: Set[Point] = Set((0, 0))
  ) {

    def score: Int = seen.size

    def isTerminal: Boolean = point == (w - 1, h - 1)

    def next: List[State] = {
      val (newCorrupt, newBytes) = bytes match
        case b :: bs => (corrupt + b, bs)
        case _       => (corrupt, bytes)

      directions
        .map { d =>
          (point._1 + d._1) -> (point._2 + d._2)
        }
        .filter { p =>
          inBounds(p) && !newCorrupt.contains(p) && !seen.contains(p)
        }
        .map { p =>
          State(newBytes, newCorrupt, p, seen + p)
        }
    }
  }

  @tailrec
  def findPath(stack: List[State], x: Option[Int] = None): Option[Int] = stack match
    case Nil => x
    case s :: ss if s.isTerminal =>
      val newScore = x.foldLeft(s.score)(Math.min)
      findPath(ss.filter(_.score < newScore), Some(newScore))
    case s :: ss => findPath(s.next ::: ss, x)
}

class RAMRun(input: List[String]) {
  private val bytes = input.map { s =>
    val ns = s.split(",").map(_.toInt)
    ns.head -> ns.tail.head
  }

  def solvePart1(): Any = findPath(List(State(bytes.take(1024)))).get
  def solvePart2(): Any = ???
}
