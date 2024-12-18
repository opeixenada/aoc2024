import RAMRun.{State, findCutoffByte, minScore, n}
import Util.{Point, readFile}

import scala.annotation.tailrec

@main def day18(): Unit = {
  val input = readFile("resources/day18")
  val solver = RAMRun(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object RAMRun {

  val n: Int = 1024
  private val side: Int = 71
  private val end: Point = (side - 1, side - 1)

  private val field: Set[Point] = (for {
    x <- 0 until side
    y <- 0 until side
  } yield (x, y)).toSet

  def inBounds(p: Point): Boolean = p._1 > -1 && p._1 < side && p._2 > -1 && p._2 < side

  val directions: List[(Int, Int)] = List((0, 1), (0, -1), (1, 0), (-1, 0))

  case class State(
      corrupt: Set[Point],
      point: Point = (0, 0),
      seen: Set[Point] = Set((0, 0)),
      score: Int = 0,
      possiblePoints: Set[Point] = field
  ) {
    private def isTerminal: Boolean = point == end

    def next: List[State] =
      if (isTerminal) Nil
      else
        directions
          .map { d =>
            (point._1 + d._1) -> (point._2 + d._2)
          }
          .filter { p =>
            inBounds(p) && possiblePoints.contains(p) && !corrupt.contains(p) && !seen.contains(p)
          }
          .map { p =>
            State(corrupt, p, seen + p, score + 1)
          }
  }

  @tailrec
  def findCutoffByte(bytes: List[Point], accBytes: Set[Point] = Set.empty, pathPoints: Set[Point] = field): Point =
    bytes match
      case Nil => throw Exception("Cutoff byte not found")
      case byte :: tail =>
        val newPathPoints = allReachable(List(State(corrupt = accBytes + byte, possiblePoints = pathPoints)))
        if (!newPathPoints.contains(end)) byte
        else findCutoffByte(tail, accBytes + byte, newPathPoints)

  @tailrec
  private def allReachable(stack: List[State], acc: Set[Point] = Set.empty): Set[Point] =
    stack match
      case Nil                              => acc
      case s :: ss if acc.contains(s.point) => allReachable(ss, acc)
      case s :: ss                          => allReachable(ss ::: s.next, acc + s.point)

  @tailrec
  def minScore(stack: List[State], scores: Map[Point, Int] = Map.empty): Option[Int] = stack match
    case Nil                                                 => scores.get(end)
    case s :: ss if scores.get(s.point).exists(_ <= s.score) => minScore(ss, scores)
    case s :: ss =>
      val newScore = scores.get(s.point).foldLeft(s.score)(Math.min)
      minScore(ss ::: s.next, scores + (s.point -> newScore))
}

class RAMRun(input: List[String]) {
  private val bytes = input.map { s =>
    val ns = s.split(",").map(_.toInt)
    ns.head -> ns.tail.head
  }

  def solvePart1(): Any = minScore(List(State(bytes.take(n).toSet))).get
  def solvePart2(): Any = {
    val byte = findCutoffByte(bytes.drop(n), accBytes = bytes.take(n).toSet)
    s"${byte._1},${byte._2}"
  }
}
