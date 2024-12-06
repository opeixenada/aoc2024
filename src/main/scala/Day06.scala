import Util.readFile

import scala.annotation.tailrec

@main def day06(): Unit = {

  val input = readFile("resources/day06").map(_.toIndexedSeq).toIndexedSeq

  def getSymbols(s: Char): Iterable[(Int, Int)] = for {
    x <- input.indices
    y <- input.head.indices
    if input(x)(y) == s
  } yield x -> y

  val start: (Int, Int) = getSymbols('^').head

  val obstacles: Set[(Int, Int)] = getSymbols('#').toSet

  def oob(xy: (Int, Int)): Boolean = {
    val (x, y) = xy
    x >= input.length || x < 0 || y >= input.head.length || y < 0
  }

  def move(xy: (Int, Int), direction: (Int, Int)): (Int, Int) = (xy._1 + direction._1, xy._2 + direction._2)

  // Part 1

  @tailrec
  def walk(xy: (Int, Int), direction: (Int, Int) = (-1, 0), seen: Set[(Int, Int)] = Set.empty): Set[(Int, Int)] =
    move(xy, direction) match
      case next if oob(next)                => seen + xy
      case next if obstacles.contains(next) => walk(xy, turnRight(direction), seen)
      case next                             => walk(next, direction, seen + xy)

  val result1 = walk(start).size

  println(result1)

  // Part 2

  @tailrec
  def isLoop(
      xy: (Int, Int),
      direction: (Int, Int) = (-1, 0),
      seen: Set[((Int, Int), (Int, Int))] = Set.empty
  )(obstacle: (Int, Int)): Boolean =
    move(xy, direction) match
      case next if oob(next)                        => false
      case next if seen.contains(next -> direction) => true
      case next if (obstacles + obstacle).contains(next) =>
        isLoop(xy, turnRight(direction), seen + (xy -> direction))(obstacle)
      case next => isLoop(next, direction, seen + (xy -> direction))(obstacle)

  val result2: Int = getSymbols('.').count(isLoop(start))

  println(result2)
}

def turnRight(direction: (Int, Int)): (Int, Int) = direction match
  case (-1, 0) => (0, 1)
  case (1, 0)  => (0, -1)
  case (0, 1)  => (1, 0)
  case _       => (-1, 0)
