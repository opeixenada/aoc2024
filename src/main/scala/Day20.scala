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

  private val directions: List[(Int, Int)] = List((0, 1), (1, 0), (0, -1), (-1, 0))

  case class Maze(walls: Set[Point], start: Point, end: Point, w: Int, h: Int) {
    val path: Map[Point, Int] = getPathsRec(List(State(start))).zipWithIndex.toMap

    private def isValid(point: Point): Boolean =
      point._1 > -1 && point._1 < w && point._2 > -1 && point._2 < h && !walls.contains(point)

    private def isWall(point: Point): Boolean = walls.contains(point)

    private def next(s: State): List[State] = getNeighbors(s.point)
      .filter(isValid)
      .map(State(_, s.point :: s.seen))

    @tailrec
    private final def getPathsRec(
        states: List[State],
        seen: List[Point] = Nil
    ): List[Point] = states match
      case s :: ss if seen.contains(s.point) => getPathsRec(ss, seen)
      case s :: ss if s.point == end         => (s.point :: s.seen).reverse
      case s :: ss                           => getPathsRec(ss ::: next(s), s.point :: seen)
      case Nil                               => throw Exception("Path not found")

    def countCheats(psToSave: Int, cheatLength: Int): Int = {
      val cheats = for {
        (pathPoint, pathPointIndex) <- path
        diffX <- (0 - cheatLength) to cheatLength
        diffY <- (0 - cheatLength) to cheatLength
        distance = diffX.abs + diffY.abs
        if distance <= cheatLength
        cheatDestination = (pathPoint._1 + diffX, pathPoint._2 + diffY)
        cheatDestinationIndex <- path.get(cheatDestination)
        cheatWin = cheatDestinationIndex - pathPointIndex - distance
        if cheatWin >= psToSave
      } yield ()

      cheats.size
    }
  }

  private def getNeighbors(point: Point): List[Point] = directions
    .map { case (x, y) => (point._1 + x) -> (point._2 + y) }

  case class State(point: Point, seen: List[Point] = Nil) {
    val ps: Int = seen.size + 1
  }
}

class RaceCondition(input: List[String]) {

  private val walls: Set[Point] = coordinatesOf('#', input).toSet
  private val start: Point = coordinatesOf('S', input).head
  private val end: Point = coordinatesOf('E', input).head

  private val maze = Maze(walls, start, end, input.head.length, input.length)

  def solvePart1(): Any = maze.countCheats(psToSave = 100, cheatLength = 2)
  def solvePart2(): Any = maze.countCheats(psToSave = 100, cheatLength = 20)
}
