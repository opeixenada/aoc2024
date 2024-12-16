import ReindeerMaze.{Maze, Point, State, solve}
import Util.{coordinatesOf, readFile, withTimeLogging}

import scala.annotation.tailrec

@main def day16(): Unit = {
  val input = readFile("resources/day16")
  val solver = ReindeerMaze(input)
  println(solver.solvePart1())
  println(solver.solvePart2())
}

object ReindeerMaze {
  type Point = (Int, Int)
  type DirectedPoint = (Point, Char)
  type Path = Set[Point]

  val up: Point => DirectedPoint = p => (p._1, p._2 - 1) -> '^'
  val down: Point => DirectedPoint = p => (p._1, p._2 + 1) -> 'v'
  val left: Point => DirectedPoint = p => (p._1 - 1, p._2) -> '<'
  val right: Point => DirectedPoint = p => (p._1 + 1, p._2) -> '>'

  val allMoves = Set(up, down, left, right)

  val oppositeMove = Map(
    '^' -> down,
    '<' -> right,
    '>' -> left,
    'v' -> up
  )

  def getMoves(direction: Char) = allMoves - oppositeMove(direction)

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
      scores: Map[Point, Int] = Map.empty.withDefaultValue(Int.MaxValue),
      paths: Set[(Path, Int)] = Set.empty
  ): (Int, Path) =
    states match
      case state :: tail if scores(state.point) < state.pathScore => solve(maze)(tail, scores, paths)

      case state :: tail if state.point == maze.end =>
        val newMinScore = Math.min(scores(state.point), state.pathScore)
        val updatedScores = scores + (state.point -> newMinScore)
        val newStates = tail.filter(_.pathScore <= newMinScore)
        val newPaths = paths + (state.path -> state.pathScore)
        solve(maze)(newStates, updatedScores, newPaths)

      case state :: tail =>
        val newMinScore = Math.min(scores(state.point), state.pathScore)
        val updatedScores = scores + (state.point -> newMinScore)
        val newStates = (tail ::: state.next(maze)).filter { state =>
          state.pathScore <= updatedScores(state.point)
        }
        solve(maze)(newStates, updatedScores, paths)

      case Nil =>
        val finalScore = scores(maze.end)
        finalScore -> paths.filter(_._2 == finalScore).flatMap(_._1)
}

class ReindeerMaze(input: List[String]) {

  val walls: Set[Point] = coordinatesOf('#', input).toSet
  val start: Point = coordinatesOf('S', input).head
  val end: Point = coordinatesOf('E', input).head

  val maze = Maze(walls, end, input.head.length, input.length)
  val state0 = State(start, '>', Set(start))

  val (score, path) = solve(maze)(List(state0))

  (0 until maze.h).foreach { y =>
    println(
      (0 until maze.w).map { x =>
        if (path.contains(x, y)) 'O'
        else if (start == (x, y)) 'S'
        else if (end == (x, y)) 'E'
        else if (walls.contains(x, y)) '#'
        else '.'
      }.mkString
    )
  }

  def solvePart1(): Any = score
  def solvePart2(): Any = path.size
}
