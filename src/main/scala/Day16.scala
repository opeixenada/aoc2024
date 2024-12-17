import ReindeerMaze.{Maze, Point, State, solve}
import Util.{coordinatesOf, readFile}

import scala.annotation.tailrec

@main def day16(): Unit = {
  val input = readFile("resources/day16")
  val solver = ReindeerMaze(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object ReindeerMaze {
  private type Point = (Int, Int)
  private type DirectedPoint = (Point, Char)
  private type Path = Set[Point]

  private val allDirections : List[Char] = List('v', '^', '<', '>')

  private val up: Point => DirectedPoint = p => (p._1, p._2 - 1) -> '^'
  private val down: Point => DirectedPoint = p => (p._1, p._2 + 1) -> 'v'
  private val left: Point => DirectedPoint = p => (p._1 - 1, p._2) -> '<'
  private val right: Point => DirectedPoint = p => (p._1 + 1, p._2) -> '>'

  private val allMoves = Set(up, down, left, right)

  private val oppositeMove = Map(
    '^' -> down,
    '<' -> right,
    '>' -> left,
    'v' -> up
  )

  private def getMoves(direction: Char) = allMoves - oppositeMove(direction)

  case class Maze(walls: Set[Point], end: Point, w: Int, h: Int) {
    def isValid(point: Point): Boolean =
      point._1 > -1 && point._1 < w && point._2 > -1 && point._2 < h && !walls.contains(point)
  }

  case class State(point: Point, direction: Char, path: Set[Point], pathScore: Int = 0) {
    def next(maze: Maze): List[State] = getMoves(direction)
      .map(_.apply(point))
      .filter { case (point, _) =>
        maze.isValid(point) && !path.contains(point)
      }
      .map { case (newPoint, newDirection) =>
        val score = if (direction != newDirection) 1001 else 1
        State(newPoint, newDirection, path + newPoint, pathScore + score)
      }
      .toList
  }

  @tailrec
  def solve(
      maze: Maze
  )(
      states: List[State],
      scores: Map[DirectedPoint, Int],
      paths: Set[(Path, Int)] = Set.empty
  ): (Int, Path) = states match
    case state :: tail =>
      val currentScore = scores(state.point -> state.direction)
      val newScore = Math.min(currentScore, state.pathScore)
      val updatedScores = scores + (state.point -> state.direction -> newScore)

      () match
        case _ if currentScore < state.pathScore =>
          solve(maze)(tail, scores, paths)

        case _ if state.point == maze.end =>
          solve(maze)(
            states = tail.filter(_.pathScore <= newScore),
            scores = updatedScores,
            paths = paths + (state.path -> state.pathScore)
          )

        case _ =>
          solve(maze)(
            states = (tail ::: state.next(maze)).filter(s => s.pathScore <= updatedScores(s.point -> s.direction)),
            scores = updatedScores,
            paths = paths
          )

    case Nil =>
      val finalScore = allDirections .map(d => scores(maze.end -> d)).min
      finalScore -> paths.filter(_._2 == finalScore).flatMap(_._1)
}

class ReindeerMaze(input: List[String]) {

  private val walls: Set[Point] = coordinatesOf('#', input).toSet
  private val start: Point = coordinatesOf('S', input).head
  private val end: Point = coordinatesOf('E', input).head

  private val maze = Maze(walls, end, input.head.length, input.length)
  private val state0 = State(start, '>', Set(start))

  private val (score, path) =
    solve(maze)(List(state0), Map(state0.point -> state0.direction -> 0).withDefaultValue(Int.MaxValue))

  def solvePart1(): Any = score
  def solvePart2(): Any = path.size
}
