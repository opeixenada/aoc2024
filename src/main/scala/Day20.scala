import RaceCondition.Maze
import Util.{Point, coordinatesOf, readFile}

import scala.annotation.tailrec

@main def day20(): Unit = {
  val input = readFile("resources/day20")
  val solver = RaceCondition(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object RaceCondition {

  private val psToSave: Int = 2

  private val directions: List[(Int, Int)] = List((0, 1), (1, 0), (0, -1), (-1, 0))

  case class Maze(walls: Set[Point], end: Point, w: Int, h: Int) {
    def isValid(point: Point): Boolean =
      point._1 > -1 && point._1 < w && point._2 > -1 && point._2 < h && !walls.contains(point)

    def isWall(point: Point): Boolean = walls.contains(point)

    def getPath(start: Point, maxTime: Option[Int] = None): Option[Set[(Int, Int)]] =
      getPathsRec(List(State(start)), maxTime)

    def next(s: State): List[State] = getNeighbors(s.point)
      .filter(isValid)
      .map(State(_, s.seen + s.point))

    @tailrec
    private final def getPathsRec(
        states: List[State],
        maxTime: Option[Int],
        seen: Set[Point] = Set.empty
    ): Option[Set[Point]] = states match
      case s :: ss if maxTime.exists(_ < s.seen.size) => None
      case s :: ss if seen.contains(s.point)          => getPathsRec(ss, maxTime, seen)
      case s :: ss if s.point == end                  => Some(s.seen)
      case s :: ss                                    => getPathsRec(ss ::: next(s), maxTime, seen + s.point)
      case Nil                                        => None

    private def getPossibleCheats(path: Set[Point]): Set[Point] = walls.filter { wall =>
      getNeighbors(wall).toSet.intersect(path + end).size > 1
    }

    def countGoodCheats(start: Point): Int = {
      val goodCheats = for {
        path <- getPath(start).toSeq
        pathTime = path.size
        cheat <- getPossibleCheats(path)
        cheatedMaze = this.copy(walls = walls - cheat)
        cheatedPath <- cheatedMaze.getPath(start, Some(pathTime - psToSave))
      } yield cheat

      goodCheats.size
    }
  }

  private def getNeighbors(point: Point): List[Point] = directions
    .map { case (x, y) => (point._1 + x) -> (point._2 + y) }

  case class State(point: Point, seen: Set[Point] = Set.empty) {
    val ps: Int = seen.size
  }
}

class RaceCondition(input: List[String]) {

  private val walls: Set[Point] = coordinatesOf('#', input).toSet
  private val start: Point = coordinatesOf('S', input).head
  private val end: Point = coordinatesOf('E', input).head

  private val maze = Maze(walls, end, input.head.length, input.length)

  def solvePart1(): Any = maze.countGoodCheats(start)
  def solvePart2(): Any = ???
}
